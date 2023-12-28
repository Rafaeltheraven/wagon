#![allow(non_snake_case)]
pub(crate) mod A_0_2;
pub(crate) mod A_0_1;
pub(crate) mod A_1_0;
pub(crate) mod A_0_0;
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct A;
impl<'a> wagon_gll::Label<'a> for A {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![
            (vec![state.get_label_by_uuid("A_0_0"), state.get_label_by_uuid("A_0_1"),
            state.get_label_by_uuid("A_0_2")], None), (vec![state
            .get_label_by_uuid("A_1_0")], None),
        ]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "A"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let mut candidates = Vec::with_capacity(2usize);
        let fst = state.get_label_by_uuid("A_0_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("A");
            let rules = state.get_rule("A_0");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "A_0_0");
            candidates.push(std::rc::Rc::new(slot));
        }
        let fst = state.get_label_by_uuid("A_1_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("A");
            let rules = state.get_rule("A_1");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "A_1_0");
            candidates.push(std::rc::Rc::new(slot));
        }
        if !candidates.is_empty() {
            let to_add = itertools::Itertools::max_set_by(
                candidates.into_iter(),
                |x, y| x.cmp(y, state),
            );
            for slot in to_add {
                state.add(slot, state.gss_pointer, state.input_pointer, state.sppf_root);
            }
        }
    }
    fn to_string(&self) -> &str {
        "A"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["A",]
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (vec![], vec![])
    }
    #[allow(unused_variables)]
    fn _weight(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        unreachable!("Weight should never be evaluated for non-zero GLL blocks")
    }
}
