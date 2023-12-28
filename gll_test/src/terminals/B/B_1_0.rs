#![allow(non_snake_case)]
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct B_1_0;
impl<'a> wagon_gll::Label<'a> for B_1_0 {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![], Some(& [])),]
    }
    fn is_eps(&self) -> bool {
        true
    }
    fn uuid(&self) -> &str {
        "B_1_0"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let cr = state.get_node_t(&[]);
        let slot = wagon_gll::GrammarSlot::new(
            state.get_label_by_uuid("B"),
            state.get_rule("B_1"),
            1,
            0,
            "B_1",
        );
        state
            .sppf_pointer = state
            .get_node_p(
                std::rc::Rc::new(slot),
                state.sppf_pointer,
                cr,
                state.gss_pointer,
            );
        state.pop(vec![None,]);
    }
    fn to_string(&self) -> &str {
        "ε"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["ε",]
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (vec!["$did_a",], vec![])
    }
    #[allow(unused_variables)]
    fn _weight(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        None
    }
}
