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
                ..
            } = test_case;

            let result = interpreter.eval(ast);

            assert!(result.is_ok());

            let output = interpreter.get_output();

            assert_eq!(*output, expected);
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
