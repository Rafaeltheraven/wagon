#![warn(missing_docs)]
//! Procedural macros for use in the WAGon suite of libraries. 
//!
//! It would make more sense to put these in the [`wagon-utils`](../wagon_utils/index.html) crate. 
//! But Rust does not allow use to export procedural macros and regular functions from the same crate. So here we are.

use proc_macro::TokenStream;
use proc_macro2::Span;
use proc_macro2::TokenStream as TokenStream2;
use syn::{Pat, Result, spanned::Spanned, ExprMacro, Attribute, DeriveInput, punctuated::Punctuated, ExprStruct, ExprCall, Expr, Token};
use quote::{quote, ToTokens, format_ident};
use syn::{parse_macro_input, ExprMatch, Arm};
use extendable_data::extendable_data;

/// A procedural macro to extend lexers from a base lexer. Used by [`wagon-lexers::math::Math`](../wagon_lexer/math/enum.Math.html) 
/// and [`wagon_lexer::productions::Productions`](wagon_lexer/productions/enum.Productions.html).
#[extendable_data(inherit_from_base)]
#[derive(Clone, Debug, PartialEq, Display, Logos)]
#[display_concat(" or ")]
#[logos(skip r"/\*([^*]|\*[^/])+\*/|[ \t\n\f]+")]
#[logos(error = LexingError)]
enum Base {
    #[display_override("Identifier")]
    #[regex("(\\$|&|\\*)?([a-zA-Z][a-zA-Z0-9_]*)", |lex| wagon_ident::Ident::detect(lex.slice(), lex.span()))]
    /// An identifier. Gets parsed to an [`wagon-ident::Ident`] automatically.
    /// An identifier may be any string of alphanumeric characters, as well as `_`. The string must start with a purely alphabetical character.
    /// The identifier may be prepended by `$`/`&`/`*` to specify what type of identifier it is.
    Identifier(wagon_ident::Ident),

    #[display_override("String Literal")]
    #[regex("\"([^\"\\\\]|\\\\.)*\"", |lex| wagon_utils::rem_first_and_last_char(lex.slice()))]
    #[regex("\'([^\'\\\\]|\\\\.)*\'", |lex| wagon_utils::rem_first_and_last_char(lex.slice()))]
    /// A string, surrounded by either `"` or `'`.
    LitString(String),

    #[token("[")]
    /// `[`
    LBr,

    #[token("]")]
    /// `]`
    RBr,

    #[token("{")]
    /// `{`
    LCur,

    #[token("}")]
    /// `}`
    RCur,

    #[token("(")]
    /// `(`
    LPar,

    #[token(")")]
    /// `)`
    RPar,

    #[token(";")]
    /// `;`
    Semi,

    #[token(":")]
    /// `:`
    Colon,

    #[token(",")]
    /// `,`
    Comma,

    #[display_override("Path")]
    #[regex(r"[a-zA-Z]*(::[a-zA-Z]*)+", |lex| lex.slice().to_string())]
    /// A Rust style path.
    Path(String),
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

#[proc_macro_derive(TokenMapper)]
/// Derive macro for the [`wagon-parser::helpers::TokenMapper`] trait.
///
/// If we have an enum that has fields with the exact same names as that of specific [`wagon-lexer::math::Math`] tokens.
/// We can automatically derive this trait to convert those tokens into instances of this enum.
pub fn derive_token_mapper(stream: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(stream as DeriveInput);
    let span = ast.span();
    let ident = ast.ident;
    let syn::Data::Enum(data) = ast.data else {
        return syn::Error::new(span, "Can only derive TokenMapper for enums").into_compile_error().into()
    };
    let mut arms: Vec<TokenStream2> = Vec::with_capacity(data.variants.len());
    for var in data.variants {
        let name = var.ident;
        arms.push(quote!(Tokens::MathToken(Math::#name) => Some(Self::#name)));
    }
    quote!(
        impl TokenMapper for #ident {
            fn token_to_enum(token: &Tokens) -> Option<Self> {
                match token {
                    #(#arms,)*
                    _ => None
                }
            }
        }
    ).into()
}

#[proc_macro_attribute]
/// Given a struct/enum with [`wagon-parser::SpannableNode`] fields, creates constructors which do not require any `SpannableNode` wrapping.
pub fn new_unspanned(_attr: TokenStream, item: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(item as DeriveInput);
    let orig_stream = quote!(#ast);
    let span = ast.span();
    let ident = ast.ident.clone();
    let data = match ast.data {
        syn::Data::Enum(e) => nonspanned_enum(e),
        syn::Data::Struct(s) => nonspanned_struct(s),
        syn::Data::Union(_) => return syn::Error::new(span, "Can only derive NonSpanned for enums and structs").into_compile_error().into()
    };
    quote!(
        #orig_stream

        impl #ident {
            #(#data)*
        }
    ).into()
}

fn nonspanned_enum(e: syn::DataEnum) -> Vec<TokenStream2> {
    let mut funcs = Vec::with_capacity(e.variants.len());
    for variant in e.variants {
        let ident = variant.ident;
        let func_name = format_ident!("new_unspanned_{}", ident.to_string().to_lowercase());
        match variant.fields {
            syn::Fields::Named(n) => {
                let mut parameters = Vec::new();
                let mut args = Vec::new();
                for field in n.named {
                    let name = field.ident.expect("Unable to get ident from field");
                    let (typ, known_iter, has_changed) = extract_spanned_node_type(field.ty, false);
                    parameters.push(quote!(#name: #typ));
                    if has_changed {
                        if known_iter {
                            args.push(quote!(#name: crate::WrapSpannable::wrap_spannable(#name)));
                        } else {
                            args.push(quote!(#name: #name.into()));
                        }
                    } else {
                        args.push(quote!(#name));
                    }
                }
                funcs.push(quote!(
                    pub(crate) fn #func_name(#(#parameters),*) -> Self {
                        Self::#ident{#(#args),*}
                    }
                ));
            },
            syn::Fields::Unnamed(u) => {
                let mut parameters = Vec::new();
                let mut args = Vec::new();
                for (i, field) in u.unnamed.into_iter().enumerate() {
                    let name = format_ident!("arg_{}", i);
                    let (typ, known_iter, has_changed) = extract_spanned_node_type(field.ty, false);
                    parameters.push(quote!(#name: #typ));
                    if has_changed {
                        if known_iter {
                            args.push(quote!(crate::WrapSpannable::wrap_spannable(#name)));
                        } else {
                            args.push(quote!(#name.into()));
                        }
                    } else {
                        args.push(quote!(#name));
                    }
                }
                funcs.push(quote!(
                    pub(crate) fn #func_name(#(#parameters),*) -> Self {
                        Self::#ident(#(#args),*)
                    }
                ));
            },
            syn::Fields::Unit => {
                funcs.push(quote!(
                    pub(crate) fn #func_name() -> Self {
                        Self::#ident
                    }
                ));
            },
        }
    }
    funcs
}

fn nonspanned_struct(s: syn::DataStruct) -> Vec<TokenStream2> {
    let mut funcs = Vec::with_capacity(1);
    match s.fields {
        syn::Fields::Named(n) => {
                let mut parameters = Vec::new();
                let mut args = Vec::new();
                for field in n.named.into_iter() {
                    let name = field.ident.expect("Unable to get ident from field");
                    let (typ, known_iter, has_changed) = extract_spanned_node_type(field.ty, false);
                    parameters.push(quote!(#name: #typ));
                    if has_changed {
                        if known_iter {
                            args.push(quote!(#name: crate::WrapSpannable::wrap_spannable(#name)));
                        } else {
                            args.push(quote!(#name: #name.into()));
                        }
                    } else {
                        args.push(quote!(#name));
                    }
                }
                funcs.push(quote!(
                    pub(crate) fn new_unspanned(#(#parameters),*) -> Self {
                        Self {#(#args),*}
                    }
                ));
            },
            syn::Fields::Unnamed(u) => {
                let mut parameters = Vec::new();
                let mut args = Vec::new();
                for (i, field) in u.unnamed.into_iter().enumerate() {
                    let name = format_ident!("arg_{}", i);
                    let (typ, known_iter, has_changed) = extract_spanned_node_type(field.ty, false);
                    parameters.push(quote!(#name: #typ));
                    if has_changed {
                        if known_iter {
                            args.push(quote!(crate::WrapSpannable::wrap_spannable(#name)));
                        } else {
                            args.push(quote!(#name.into()));
                        }
                    } else {
                        args.push(quote!(#name));
                    }
                }
                funcs.push(quote!(
                    pub(crate) fn new_unspanned(#(#parameters),*) -> Self {
                        Self (#(#args),*)
                    }
                ));
            },
            syn::Fields::Unit => {
                funcs.push(quote!(
                    pub(crate) fn new_unspanned() -> Self {
                        Self
                    }
                ));
            },
    }
    funcs
}

const CUSTOM_INTO: [&str; 4] = ["Vec", "Option", "Box", "BTreeMap"];
const IGNORE_UNSPAN: [&str; 4] = ["Terminal", "Ident", "Some", "None"];

fn extract_spanned_node_type(root: syn::Type, mut known_custom: bool) -> (syn::Type, bool, bool) {
    match root {
        syn::Type::Path(mut p) => {
            let mut has_changed = false;
            for segment in p.path.segments.iter_mut() {
                let ident = segment.ident.to_string();
                if ident == "SpannableNode" {
                    if let syn::PathArguments::AngleBracketed(b) = &segment.arguments {
                        for arg in b.args.iter() {
                            if let syn::GenericArgument::Type(t) = arg {
                                return (t.clone(), known_custom, true)
                            }
                        }
                    }
                } else if let syn::PathArguments::AngleBracketed(b) = &mut segment.arguments {
                    let mut new_args = Punctuated::new();
                    for arg in &b.args {
                        match arg {
                            syn::GenericArgument::Type(t) => {
                                let (new_type, new_known, new_changed) = extract_spanned_node_type(t.clone(), CUSTOM_INTO.contains(&ident.as_str()));
                                (known_custom, has_changed) = (new_known, new_changed);
                                new_args.push(
                                    syn::GenericArgument::Type(
                                        new_type
                                    )
                                );
                            },
                            other => new_args.push(other.clone()),
                        }
                    }
                    b.args = new_args;
                }
            }
            (syn::Type::Path(p), known_custom, has_changed)
        },
        other => (other, false, false)
    }
}

#[proc_macro]
/// Given a hand-written, complete [wagon-parser::Wag] without any Span information, creates a proper one with dummy Span data.
pub fn unspanned_tree(stream: TokenStream) -> TokenStream {
    let ast = parse_macro_input!(stream as ExprStruct);
    match unspanned_struct(ast) {
        Ok(s) => s,
        Err(e) => e.into_compile_error(),
    }.into()
}

fn unspanned_struct(ast: ExprStruct) -> Result<TokenStream2> {
    let mut path_iter = ast.path.segments.into_iter();
    let main_ident = path_iter.next().expect("Got an empty path").ident;
    let mut args = Vec::new();
    for field in ast.fields {
        args.push(handle_expr(field.expr)?);
    }
    Ok(quote!(
        #main_ident::new_unspanned(#(#args),*)
    ))
}

fn unspanned_enum(ast: ExprCall) -> Result<TokenStream2> {
    let span = ast.span();
    let path = match *ast.func {
        syn::Expr::Path(p) => p.path,
        other => return Err(syn::Error::new(span, format!("Expected path for an enum. Got {:?}", other))),
    };
    let mut path_iter = path.segments.into_iter();
    let main_ident = path_iter.next().expect("Got an empty path").ident;
    let func_path = if let Some(sub_path) = path_iter.next() {
        let ident_string = sub_path.ident.to_string();
        let func_path = if !IGNORE_UNSPAN.contains(&main_ident.to_string().as_str()) && ident_string.chars().next().expect("Got an empty ident_string").is_uppercase() {
            format_ident!("new_unspanned_{}", ident_string.to_lowercase())
        } else {
            sub_path.ident
        };
        quote!(#main_ident::#func_path)
    } else { // Tuple structs AAAAAAAAAAAAAAAAAAAAA
        if !IGNORE_UNSPAN.contains(&main_ident.to_string().as_str()) {
            quote!(#main_ident::new_unspanned)
        } else {
            main_ident.into_token_stream()
        }
    };
    let mut args = Vec::new();
    for expr in ast.args {
        args.push(handle_expr(expr)?);
    }
    Ok(quote!(
        #func_path(#(#args),*)
    ))
}

fn unspanned_macro(ast: ExprMacro) -> Result<TokenStream2> {
    let exprs = ast.mac.parse_body_with(Punctuated::<Expr, Token![,]>::parse_terminated)?;
    let (path, bang, delim) = (ast.mac.path, ast.mac.bang_token, ast.mac.delimiter);
    let mut args = Vec::new();
    for expr in exprs {
        args.push(handle_expr(expr)?);
    }
    let arg_stream = quote!(#(#args,)*);
    let mut surround_stream = TokenStream2::new();
    match delim {
        syn::MacroDelimiter::Paren(p) => p.surround(&mut surround_stream, |x| x.extend(arg_stream)),
        syn::MacroDelimiter::Brace(b) => b.surround(&mut surround_stream, |x| x.extend(arg_stream)),
        syn::MacroDelimiter::Bracket(b) => b.surround(&mut surround_stream, |x| x.extend(arg_stream)),
    }
    Ok(quote!(
        #path #bang #surround_stream
    ))
}

fn handle_expr(expr: Expr) -> Result<TokenStream2> {
    let span = expr.span();
    match expr {
        syn::Expr::Call(c) => unspanned_enum(c),
        syn::Expr::Macro(m) => unspanned_macro(m),
        syn::Expr::Struct(s) => unspanned_struct(s),
        syn::Expr::MethodCall(m) => Ok(m.to_token_stream()),
        syn::Expr::Path(p) => Ok(p.to_token_stream()),
        syn::Expr::Lit(l) => Ok(l.to_token_stream()),
        syn::Expr::Tuple(t) => Ok(t.to_token_stream()),
        syn::Expr::Array(a) => Ok(a.to_token_stream()),
        other => Err(syn::Error::new(span, format!("Unexpected Expression {:?}", other)))
    }
}

#[proc_macro]
/// Automatically return a parse error if an unexpected token is encountered.
///
/// Put around a match statement. This will automatically create a catch-all that, if matched, returns an error. 
/// You can annotate a match arm with the attribute `#[expect()]` in order to specify the string representation of that arm. 
/// Otherwise, the string representation of whatever is on the left side of the arm is taken.
///
/// This macro is intended specifically for [wagon-parser](../wagon_parser/index.html) and thus expects a lexer to be present and returns a specific error. 
/// This macro is not intended to be used for any other match statements.
/// If you do want to use it, make sure there is a variable called `lexer` with the function `span` and an enum `WagParseError` with variant `Unexpected`,
/// which has the fields `span`, `offender` and `expected`.
///
/// # Example
/// ```
/// # struct Lexer;
/// # impl Lexer {
/// #     fn span(&self) -> i32 {
/// #         0        
/// #     }
/// # }
/// # enum WagParseError {
/// #     Unexpected {
/// #         span: i32,
/// #         offender: A,
/// #         expected: Vec<String>
/// #     }   
/// # }
/// use wagon_macros::match_error;
/// enum A {
///    One,
///    Two,
///    Three
/// };
/// impl std::fmt::Display for A {
/// # fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result { write!(f, "{}", "Bar") } 
/// }
/// let a = A::One;
/// let lexer = Lexer;
/// assert!(match_error!(match a {
///     #[expect("Foo")]
///     A::Two => Ok(()),
///     A::Three => Ok(())
/// }).is_err());
/// ```
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
                Err(e) => return e.into_compile_error().into()
            }
        }
    }
    let joined = quote! {
        #( #expected ),*
    };
    let wc_arm: Arm = syn::parse_quote!(
        _error => {
            Err(WagParseError::Unexpected{span: lexer.span(), offender: _error, expected: vec![#joined]})
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
        Pat::Or(o) => o.cases.iter().map(|x| pat_to_str(x, span)).collect(),
        Pat::Paren(p) => pat_to_str(&p.pat, span),
        Pat::Path(p) => Ok(p.path.to_token_stream()),
        Pat::Reference(r) => pat_to_str(&r.pat, span),
        Pat::Tuple(t) => t.elems.iter().map(|x| pat_to_str(x, span)).collect(),
        Pat::TupleStruct(t) => {
            let mut sub_list: Vec<TokenStream2> = Vec::new();
            for elem in t.elems.iter() {
                if let Pat::Ident(_) = elem {
                    sub_list.push(quote!(Default::default()))
                } else {
                    sub_list.push(pat_to_str(elem, span)?);
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
