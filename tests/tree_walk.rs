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

            interpreter.eval(ast);

            let result = String::from_utf8(interpreter.get_writer().clone()).unwrap();

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
