#![allow(non_snake_case)]
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct E_3_0;
impl<'a> wagon_gll::Label<'a> for E_3_0 {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![state.get_label_by_uuid("D")], None),]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "E_3_0"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let i_did_a = state.get_attribute(0usize).to_owned();
        let label = state.get_label(&wagon_ident::Ident::Unknown("D".to_string()));
        state
            .gss_pointer = state
            .create(
                std::rc::Rc::new(
                    wagon_gll::GrammarSlot::new(
                        state.get_label_by_uuid("E"),
                        state.get_rule("E_3"),
                        1usize,
                        0,
                        "E_3",
                    ),
                ),
                vec![i_did_a,],
            );
        label.code(state);
    }
    fn to_string(&self) -> &str {
        "D"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["D",]
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (vec!["*did_a",], vec![])
    }
    #[allow(unused_variables)]
    fn _weight(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        let i_did_a = state.get_attribute(0usize).to_owned();
        Some(!i_did_a)
    }
}
