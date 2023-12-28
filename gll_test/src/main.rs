mod terminals;
#[derive(Debug)]
struct _S;
impl<'a> wagon_gll::Label<'a> for _S {
    fn first_set(
        &self,
        state: &wagon_gll::state::GLLState<'a>,
    ) -> Vec<(Vec<wagon_gll::GLLBlockLabel<'a>>, Option<wagon_gll::Terminal<'a>>)> {
        vec![(vec![state.get_label_by_uuid("S")], None)]
    }
    fn is_eps(&self) -> bool {
        false
    }
    fn uuid(&self) -> &str {
        wagon_gll::ROOT_UUID
    }
    fn to_string(&self) -> &str {
        wagon_gll::ROOT_UUID
    }
    fn str_parts(&self) -> Vec<&str> {
        vec![wagon_gll::ROOT_UUID]
    }
    fn code(&self, _: &mut wagon_gll::state::GLLState<'a>) {
        unreachable!("This should never be called");
    }
    fn attr_rep_map(&self) -> (Vec<&str>, Vec<&str>) {
        (Vec::new(), Vec::new())
    }
    fn _weight(
        &self,
        _state: &wagon_gll::state::GLLState<'a>,
    ) -> Option<wagon_gll::value::Value<'a>> {
        unreachable!("This should never be called");
    }
}
#[allow(non_snake_case)]
fn main() {
    let args = clap::command!()
        .arg(
            clap::arg!(< filename > "Input file to parse")
                .value_parser(clap::value_parser!(std::path::PathBuf)),
        )
        .arg(clap::arg!(- - "no-crop" "Don't crop resulting sppf").num_args(0))
        .get_matches();
    let input_file = args
        .get_one::<std::path::PathBuf>("filename")
        .expect("Input file required");
    let crop = args.get_one::<bool>("no-crop").unwrap() == &false;
    let content_string = std::fs::read_to_string(input_file)
        .expect("Couldn't read file");
    let contents = content_string.trim_end().as_bytes();
    let mut label_map: std::collections::HashMap<
        &str,
        std::rc::Rc<dyn wagon_gll::Label>,
    > = std::collections::HashMap::with_capacity(42usize);
    let mut rule_map: std::collections::HashMap<
        &str,
        std::rc::Rc<Vec<wagon_ident::Ident>>,
    > = std::collections::HashMap::with_capacity(8usize);
    let label_0 = std::rc::Rc::new(terminals::E::E_2_0::E_2_0 {});
    label_map.insert("E_2_0", label_0);
    let label_1 = std::rc::Rc::new(terminals::E::E {});
    label_map.insert("E", label_1);
    let alt_E_0 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("E_0_0".to_string()),
            wagon_ident::Ident::Unknown("E_0_1".to_string()),
        ],
    );
    rule_map.insert("E_0", alt_E_0);
    let alt_E_1 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("E_1_0".to_string()),
            wagon_ident::Ident::Unknown("E_1_1".to_string()),
            wagon_ident::Ident::Unknown("E_1_2".to_string()),
            wagon_ident::Ident::Unknown("E_1_3".to_string()),
        ],
    );
    rule_map.insert("E_1", alt_E_1);
    let alt_E_2 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("E_2_0".to_string()),
            wagon_ident::Ident::Unknown("E_2_1".to_string()),
        ],
    );
    rule_map.insert("E_2", alt_E_2);
    let alt_E_3 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("E_3_0".to_string()),
            wagon_ident::Ident::Unknown("E_3_1".to_string()),
            wagon_ident::Ident::Unknown("E_3_2".to_string()),
            wagon_ident::Ident::Unknown("E_3_3".to_string()),
        ],
    );
    rule_map.insert("E_3", alt_E_3);
    let label_2 = std::rc::Rc::new(terminals::S::S_0_0::S_0_0 {});
    label_map.insert("S_0_0", label_2);
    let label_3 = std::rc::Rc::new(terminals::B::B {});
    label_map.insert("B", label_3);
    let alt_B_0 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("B_0_0".to_string()),
            wagon_ident::Ident::Unknown("B_0_1".to_string()),
            wagon_ident::Ident::Unknown("B_0_2".to_string()),
        ],
    );
    rule_map.insert("B_0", alt_B_0);
    let alt_B_1 = std::rc::Rc::new(
        vec![wagon_ident::Ident::Unknown("B_1_0".to_string()),],
    );
    rule_map.insert("B_1", alt_B_1);
    let label_4 = std::rc::Rc::new(terminals::E::E_1_3::E_1_3 {});
    label_map.insert("E_1_3", label_4);
    let label_5 = std::rc::Rc::new(terminals::G::G {});
    label_map.insert("G", label_5);
    let alt_G_0 = std::rc::Rc::new(
        vec![wagon_ident::Ident::Unknown("G_0_0".to_string()),],
    );
    rule_map.insert("G_0", alt_G_0);
    let label_6 = std::rc::Rc::new(terminals::G::G_0_0::G_0_0 {});
    label_map.insert("G_0_0", label_6);
    let label_7 = std::rc::Rc::new(terminals::E::E_1_1::E_1_1 {});
    label_map.insert("E_1_1", label_7);
    let label_8 = std::rc::Rc::new(terminals::C::C_1_0::C_1_0 {});
    label_map.insert("C_1_0", label_8);
    let label_9 = std::rc::Rc::new(terminals::F::F {});
    label_map.insert("F", label_9);
    let alt_F_0 = std::rc::Rc::new(
        vec![wagon_ident::Ident::Unknown("F_0_0".to_string()),],
    );
    rule_map.insert("F_0", alt_F_0);
    let label_10 = std::rc::Rc::new(terminals::S::S_0_1::S_0_1 {});
    label_map.insert("S_0_1", label_10);
    let label_11 = std::rc::Rc::new(terminals::A::A {});
    label_map.insert("A", label_11);
    let alt_A_0 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("A_0_0".to_string()),
            wagon_ident::Ident::Unknown("A_0_1".to_string()),
            wagon_ident::Ident::Unknown("A_0_2".to_string()),
        ],
    );
    rule_map.insert("A_0", alt_A_0);
    let alt_A_1 = std::rc::Rc::new(
        vec![wagon_ident::Ident::Unknown("A_1_0".to_string()),],
    );
    rule_map.insert("A_1", alt_A_1);
    let label_12 = std::rc::Rc::new(terminals::E::E_3_0::E_3_0 {});
    label_map.insert("E_3_0", label_12);
    let label_13 = std::rc::Rc::new(terminals::C::C_1_1::C_1_1 {});
    label_map.insert("C_1_1", label_13);
    let label_14 = std::rc::Rc::new(terminals::S::S_1_0::S_1_0 {});
    label_map.insert("S_1_0", label_14);
    let label_15 = std::rc::Rc::new(terminals::E::E_2_1::E_2_1 {});
    label_map.insert("E_2_1", label_15);
    let label_16 = std::rc::Rc::new(terminals::E::E_3_3::E_3_3 {});
    label_map.insert("E_3_3", label_16);
    let label_17 = std::rc::Rc::new(terminals::B::B_0_2::B_0_2 {});
    label_map.insert("B_0_2", label_17);
    let label_18 = std::rc::Rc::new(terminals::B::B_0_1::B_0_1 {});
    label_map.insert("B_0_1", label_18);
    let label_19 = std::rc::Rc::new(terminals::B::B_1_0::B_1_0 {});
    label_map.insert("B_1_0", label_19);
    let label_20 = std::rc::Rc::new(terminals::E::E_0_0::E_0_0 {});
    label_map.insert("E_0_0", label_20);
    let label_21 = std::rc::Rc::new(terminals::D::D {});
    label_map.insert("D", label_21);
    let alt_D_0 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("D_0_0".to_string()),
            wagon_ident::Ident::Unknown("D_0_1".to_string()),
        ],
    );
    rule_map.insert("D_0", alt_D_0);
    let alt_D_1 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("D_1_0".to_string()),
            wagon_ident::Ident::Unknown("D_1_1".to_string()),
        ],
    );
    rule_map.insert("D_1", alt_D_1);
    let label_22 = std::rc::Rc::new(terminals::D::D_0_0::D_0_0 {});
    label_map.insert("D_0_0", label_22);
    let label_23 = std::rc::Rc::new(terminals::E::E_0_1::E_0_1 {});
    label_map.insert("E_0_1", label_23);
    let label_24 = std::rc::Rc::new(terminals::C::C {});
    label_map.insert("C", label_24);
    let alt_C_0 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("C_0_0".to_string()),
            wagon_ident::Ident::Unknown("C_0_1".to_string()),
        ],
    );
    rule_map.insert("C_0", alt_C_0);
    let alt_C_1 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("C_1_0".to_string()),
            wagon_ident::Ident::Unknown("C_1_1".to_string()),
        ],
    );
    rule_map.insert("C_1", alt_C_1);
    let label_25 = std::rc::Rc::new(terminals::D::D_1_0::D_1_0 {});
    label_map.insert("D_1_0", label_25);
    let label_26 = std::rc::Rc::new(terminals::A::A_0_2::A_0_2 {});
    label_map.insert("A_0_2", label_26);
    let label_27 = std::rc::Rc::new(terminals::S::S {});
    label_map.insert("S", label_27);
    let alt_S_0 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("S_0_0".to_string()),
            wagon_ident::Ident::Unknown("S_0_1".to_string()),
        ],
    );
    rule_map.insert("S_0", alt_S_0);
    let alt_S_1 = std::rc::Rc::new(
        vec![
            wagon_ident::Ident::Unknown("S_1_0".to_string()),
            wagon_ident::Ident::Unknown("S_1_1".to_string()),
        ],
    );
    rule_map.insert("S_1", alt_S_1);
    label_map.insert(wagon_gll::ROOT_UUID, std::rc::Rc::new(_S {}));
    rule_map
        .insert(
            wagon_gll::ROOT_UUID,
            std::rc::Rc::new(vec![wagon_ident::Ident::Unknown("S".to_string())]),
        );
    let label_28 = std::rc::Rc::new(terminals::C::C_0_0::C_0_0 {});
    label_map.insert("C_0_0", label_28);
    let label_29 = std::rc::Rc::new(terminals::D::D_0_1::D_0_1 {});
    label_map.insert("D_0_1", label_29);
    let label_30 = std::rc::Rc::new(terminals::F::F_0_0::F_0_0 {});
    label_map.insert("F_0_0", label_30);
    let label_31 = std::rc::Rc::new(terminals::E::E_1_0::E_1_0 {});
    label_map.insert("E_1_0", label_31);
    let label_32 = std::rc::Rc::new(terminals::E::E_1_2::E_1_2 {});
    label_map.insert("E_1_2", label_32);
    let label_33 = std::rc::Rc::new(terminals::E::E_3_2::E_3_2 {});
    label_map.insert("E_3_2", label_33);
    let label_34 = std::rc::Rc::new(terminals::S::S_1_1::S_1_1 {});
    label_map.insert("S_1_1", label_34);
    let label_35 = std::rc::Rc::new(terminals::A::A_0_1::A_0_1 {});
    label_map.insert("A_0_1", label_35);
    let label_36 = std::rc::Rc::new(terminals::A::A_1_0::A_1_0 {});
    label_map.insert("A_1_0", label_36);
    let label_37 = std::rc::Rc::new(terminals::E::E_3_1::E_3_1 {});
    label_map.insert("E_3_1", label_37);
    let label_38 = std::rc::Rc::new(terminals::A::A_0_0::A_0_0 {});
    label_map.insert("A_0_0", label_38);
    let label_39 = std::rc::Rc::new(terminals::B::B_0_0::B_0_0 {});
    label_map.insert("B_0_0", label_39);
    let label_40 = std::rc::Rc::new(terminals::D::D_1_1::D_1_1 {});
    label_map.insert("D_1_1", label_40);
    let label_41 = std::rc::Rc::new(terminals::C::C_0_1::C_0_1 {});
    label_map.insert("C_0_1", label_41);
    let mut state = wagon_gll::state::GLLState::init(&contents, label_map, rule_map);
    state.main();
    println!("{}", state.print_sppf_dot(crop));
    assert!(state.accepts());
}
