#![allow(non_snake_case)]
pub(crate) mod E_2_0;
pub(crate) mod E_1_3;
pub(crate) mod E_1_1;
pub(crate) mod E_3_0;
pub(crate) mod E_2_1;
pub(crate) mod E_3_3;
pub(crate) mod E_0_0;
pub(crate) mod E_0_1;
pub(crate) mod E_1_0;
pub(crate) mod E_1_2;
pub(crate) mod E_3_2;
pub(crate) mod E_3_1;
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct E;
impl<'a> wagon_gll::Label<'a> for E {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![
            (vec![state.get_label_by_uuid("E_0_0"), state.get_label_by_uuid("E_0_1")],
            None), (vec![state.get_label_by_uuid("E_1_0"), state
            .get_label_by_uuid("E_1_1"), state.get_label_by_uuid("E_1_2"), state
            .get_label_by_uuid("E_1_3")], None), (vec![state.get_label_by_uuid("E_2_0"),
            state.get_label_by_uuid("E_2_1")], None), (vec![state
            .get_label_by_uuid("E_3_0"), state.get_label_by_uuid("E_3_1"), state
            .get_label_by_uuid("E_3_2"), state.get_label_by_uuid("E_3_3")], None),
        ]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "E"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let mut candidates = Vec::with_capacity(4usize);
        let fst = state.get_label_by_uuid("E_0_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("E");
            let rules = state.get_rule("E_0");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "E_0_0");
            candidates.push(std::rc::Rc::new(slot));
        }
        let fst = state.get_label_by_uuid("E_1_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("E");
            let rules = state.get_rule("E_1");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "E_1_0");
            candidates.push(std::rc::Rc::new(slot));
        }
        let fst = state.get_label_by_uuid("E_2_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("E");
            let rules = state.get_rule("E_2");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "E_2_0");
            candidates.push(std::rc::Rc::new(slot));
        }
        let fst = state.get_label_by_uuid("E_3_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("E");
            let rules = state.get_rule("E_3");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "E_3_0");
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
        "E"
    }
    fn str_parts(&self) -> Vec<&str> {
        vec!["E",]
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
