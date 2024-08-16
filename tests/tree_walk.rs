mod tests;

use crate::tests::TestCase;

make_interpreter_test!(assignments);
make_interpreter_test!(basic_statements);
make_interpreter_test!(reassignment);
make_interpreter_test!(scope);
make_interpreter_test!(if_true);
make_interpreter_test!(if_false);
make_interpreter_test!(basic_logic);
make_interpreter_test!(basic_while);
make_interpreter_test!(for_loop);
make_interpreter_test!(builtin_call, |output: &String| {
    let current_time = chrono::offset::Local::now().timestamp_millis();
    let received_time = output.trim().parse::<i64>();

    assert!(received_time.is_ok());

    let diff = received_time.unwrap() - current_time;

    assert!(
        diff < 10, // idk, arbitrary
        "Received large diff between current time and parsed time: {}",
        diff
    );
});
make_interpreter_test!(basic_function_call);
make_interpreter_test!(return_stmt_basic);

// these two tests fail for the current tree-walk implementation
// and we abandoned the tree-walk implementation!
make_interpreter_test!(return_stmt);
make_interpreter_test!(closures);

make_interpreter_test!(scope_update);
make_interpreter_test!(resolving_and_binding);
