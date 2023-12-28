#![allow(non_snake_case)]
pub(crate) mod F_0_0;
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct F;
impl<'a> wagon_gll::Label<'a> for F {
    #[allow(unused_variables)]
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![state.get_label_by_uuid("F_0_0")], None),]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        "F"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        let mut candidates = Vec::with_capacity(1usize);
        let fst = state.get_label_by_uuid("F_0_0");
        if state.test_next(fst) {
            let root = state.get_label_by_uuid("F");
            let rules = state.get_rule("F_0");
            let slot = wagon_gll::GrammarSlot::new(root, rules, 0, 0, "F_0_0");
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
        unreachable!("Weight should never be evaluated for non-zero GLL blocks")
    }
}
