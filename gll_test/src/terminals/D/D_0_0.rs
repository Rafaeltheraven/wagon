#![allow(non_snake_case)]
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct D_0_0;
impl<'a> wagon_gll::Label<'a> for D_0_0 {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![state.get_label_by_uuid("F")], None),]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "D_0_0"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let label = state.get_label(&wagon_ident::Ident::Unknown("F".to_string()));
        state
            .gss_pointer = state
            .create(
                std::rc::Rc::new(
                    wagon_gll::GrammarSlot::new(
                        state.get_label_by_uuid("D"),
                        state.get_rule("D_0"),
                        1usize,
                        0,
                        "D_0",
                    ),
                ),
                vec![],
            );
        label.code(state);
    }
    fn to_string(&self) -> &str {
        "F"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["F",]
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (vec![], vec![])
    }
    #[allow(unused_variables)]
    fn _weight(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        Some(0.7f32.into())
    }
}
