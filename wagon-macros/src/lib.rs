
use proc_macro2::{Span};
use syn::{Pat, Result, spanned::Spanned, ExprMacro, Attribute};
use extendable_data::extendable_data;
use quote::ToTokens;
use syn::{parse_macro_input, ExprMatch, Arm};

#[extendable_data(inherit_from_base)]
#[derive(Clone, Debug, PartialEq, Display, Logos)]
#[display_concat(" or ")]
#[logos(skip r"[ \t\n\f]+")]
enum Base {
    #[regex("(\\$|!)?([a-zA-Z][a-zA-Z0-9]*)", |lex| Ident::detect(lex.slice()))]
    Identifier(Ident),

    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| rem_first_and_last_char(lex.slice()))]
    #[regex("\'([^\'\\\\]|\\\\.)*\'", |lex| rem_first_and_last_char(lex.slice()))]
    LitString(String),

    #[token("[")]
    LBr,

    #[token("]")]
    RBr,

    #[token("{")]
    LCur,

    #[token("}")]
    RCur,
}

struct Args {
    message: String,
}

impl Default for Args {
    fn default() -> Self {
        Self {
            message: "Unexpected token {}, expecting {}".to_string(),
        }
    }
}

fn pop_attr(attrs: &mut Vec<Attribute>, key: &str) -> Option<TokenStream2> {
    let mut found = None;
    for (i, attr) in attrs.iter_mut().enumerate() {
        if let syn::Meta::List(l) = &attr.meta {
            if l.path.is_ident(key) {
                found = Some(i);
                break;
            }
        }
    }
    if let Some(i) = found {
        if let syn::Meta::List(l) = attrs.remove(i).meta {
            return Some(l.tokens)
        }
    }
    None
}

#[proc_macro]
pub fn match_error(stream: TokenStream) -> TokenStream {
    let mut ast = parse_macro_input!(stream as ExprMatch);
    let mut expected: Vec<TokenStream2> = Vec::new();
    for arm in ast.arms.iter_mut() {
        let span = arm.span();
        if let Some(expect) = pop_attr(&mut arm.attrs, "expect") {
            expected.push(quote!(#expect.to_string()));
        } else {
            let resp = pat_to_str(&arm.pat, span);
            match resp {
                Ok(stream) => expected.push(quote!(#stream.to_string())),
                Err(e) => return e.to_compile_error().into()
            }
        }
    }
    let joined = quote! {
        #( #expected ),*
    };
    let mut args = Args::default();
    if let Some(message) = pop_attr(&mut ast.attrs, "message") {
        args.message = message.to_string();
    }
    let message = args.message;
    let expr = &ast.expr;
    let wc_arm: Arm = syn::parse_quote!(
        _ => {
            let mut expected = [#joined];
            expected[expected.len() - 1] = format!("or {}", expected[expected.len() - 1]);
            Err(format!(#message, #expr, expected.join(", ")))
        }
    );
    ast.arms.push(wc_arm);
    ast.into_token_stream().into()
}

#[cfg(feature = "nightly")]
fn pat_stream_to_str(stream: TokenStream2, span: Span) -> Result<TokenStream2> {
    let pat: Pat = syn::parse::Parser::parse2(Pat::parse_multi_with_leading_vert, stream)?;
    pat_to_str(pat, span)
}

#[cfg(feature = "nightly")]
fn handle_macro(m: &ExprMacro, span: Span) -> Result<TokenStream2> {
    let conv: TokenStream = m.mac.to_token_stream().into();
    let expanded = conv.expand_expr();
    match expanded {
        Ok(exp) => pat_stream_to_str(exp.into(), span),
        Err(e) => Err(syn::Error::new(span, e)),
    }
}

#[cfg(not(feature = "nightly"))]
fn handle_macro(m: &ExprMacro, _span: Span) -> Result<TokenStream2> {
    Ok(m.mac.to_token_stream())
}

fn pat_to_str(pat: &Pat, span: Span) -> Result<TokenStream2> {
    match pat {
        Pat::Ident(p) => Ok(p.ident.to_token_stream()),
        Pat::Lit(l) => Ok(l.lit.to_token_stream()),
        Pat::Macro(m) => handle_macro(m, span),
        Pat::Or(o) => o.cases.iter().map(|x| pat_to_str(&x, span)).collect(),
        Pat::Paren(p) => pat_to_str(&p.pat, span),
        Pat::Path(p) => Ok(p.path.to_token_stream()),
        Pat::Reference(r) => pat_to_str(&r.pat, span),
        Pat::Tuple(t) => t.elems.iter().map(|x| pat_to_str(&x, span)).collect(),
        Pat::TupleStruct(t) => {
            let mut sub_list: Vec<TokenStream2> = Vec::new();
            for elem in t.elems.iter() {
                if let Pat::Ident(_) = elem {
                    sub_list.push(quote!(Default::default()))
                } else {
                    sub_list.push(pat_to_str(&elem, span)?);
                }
            };
            let main = &t.path;
            if !sub_list.is_empty() {
                let sub: TokenStream2 = sub_list.into_iter().collect();
                Ok(quote!{#main(#sub)})
            } else {
                Ok(quote!(#main))
            }
        },
        Pat::Type(t) => pat_to_str(&t.pat, span),
        Pat::Verbatim(v) => Ok(v.clone()),
        Pat::Wild(_) => Err(syn::Error::new(span, "Match statement already has a wildcard pattern, can not add another!")),
        p => Err(syn::Error::new(span, format!("Unsupported pattern! {:?}", p))),
    }
}