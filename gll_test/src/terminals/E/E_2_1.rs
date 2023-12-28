#![allow(non_snake_case)]
#[derive(Debug)]
#[allow(non_camel_case_types)]
pub(crate) struct E_2_1;
impl<'a> wagon_gll::Label<'a> for E_2_1 {
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
        "E_2_1"
    }
    #[allow(unused_variables)]
    fn code(&self, state: &mut wagon_gll::state::GLLState<'a>) {
        state.pop(vec![None,]);
    }
    fn to_string(&self) -> &str {
        ""
    }
    fn str_parts(&self) -> Vec<&str> {
        vec![]
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
