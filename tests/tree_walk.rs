mod tests;

use tests::TestCase;

macro_rules! make_test {
    ($name:ident) => {
        #[test]
        fn $name() {
            let test_case = TestCase::new(stringify!($name)).unwrap();

            let TestCase {
                mut interpreter,
                ast,
                expected,
                writer,
                ..
            } = test_case;

            let _ = interpreter.eval(ast).unwrap();

            let result = *writer;

            assert_eq!(result, expected);
        }
    };
}

make_test!(assignments);
make_test!(basic_statements);
make_test!(reassignment);
make_test!(scope);
make_test!(if_true);
make_test!(if_false);
make_test!(basic_logic);
make_test!(basic_while);
make_test!(for_loop);
