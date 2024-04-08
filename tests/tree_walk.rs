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

            assert!(result.is_ok(), "received err result: {:?}", result);

            let output = interpreter.get_output();

            assert_eq!(*output, expected);
        }
    };

    ($name:ident, $override:expr) => {
        #[test]
        fn $name() {
            let test_case = TestCase::new(stringify!($name)).unwrap();

            let TestCase {
                mut interpreter,
                ast,
                ..
            } = test_case;

            let result = interpreter.eval(ast);

            assert!(result.is_ok());

            let output = interpreter.get_output();

            $override(output);
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
make_test!(builtin_call, |output: &String| {
    let current_time = chrono::offset::Local::now().timestamp_millis();
    let received_time = output.trim().parse::<i64>();

    assert!(received_time.is_ok());

    let diff = received_time.unwrap() - current_time;

    assert!(
        diff < 10, // idk, arbitrary
        "Recieved large diff between current time and parsed time: {}",
        diff
    );
});
make_test!(basic_function_call);
make_test!(return_stmt);
make_test!(closures);
