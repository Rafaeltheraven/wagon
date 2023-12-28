#![allow(non_snake_case)]
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct G_0_0;
impl<'a> wagon_gll::Label<'a> for G_0_0 {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![], Some(b"1")),]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "G_0_0"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let bytes = b"1";
        let node = state.get_node_t(bytes);
        state.next(bytes).unwrap();
        let slot = wagon_gll::GrammarSlot::new(
            state.get_label_by_uuid("G"),
            state.get_rule("G_0"),
            1usize,
            0usize,
            "G_0",
        );
        state
            .sppf_pointer = state
            .get_node_p(
                std::rc::Rc::new(slot),
                state.sppf_pointer,
                node,
                state.gss_pointer,
            );
        state.pop(vec![]);
    }
    fn to_string(&self) -> &str {
        "'1'"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["'1'",]
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (vec![], vec![])
    }
    #[allow(unused_variables)]
    fn _weight(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        None
    }
}
