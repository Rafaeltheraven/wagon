#![allow(non_snake_case)]
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct E_1_2;
impl<'a> wagon_gll::Label<'a> for E_1_2 {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![state.get_label_by_uuid("C")], None),]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "E_1_2"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let i_did_a = state.restore_attribute(0usize).to_owned();
        let label = state.get_label(&wagon_ident::Ident::Unknown("C".to_string()));
        if state.test_next(label.clone()) {
            state
                .gss_pointer = state
                .create(
                    std::rc::Rc::new(
                        wagon_gll::GrammarSlot::new(
                            state.get_label_by_uuid("E"),
                            state.get_rule("E_1"),
                            3usize,
                            0,
                            "E_1",
                        ),
                    ),
                    vec![i_did_a,],
                );
            label.code(state);
        } else {
            return;
        }
    }
    fn to_string(&self) -> &str {
        "C"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["C",]
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (vec![], vec!["*did_a",])
    }
    #[allow(unused_variables)]
    fn _weight(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        unreachable!("Weight should never be evaluated for non-zero GLL blocks")
    }
}
