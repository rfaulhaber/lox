mod tests;

use crate::tests::TestCase;

// tree walk implementation was abandoned, and most of the tests don't work
// so most of them are ignored

make_tree_walk_test!(assignments);
make_tree_walk_test!(basic_statements);
make_tree_walk_test!(reassignment);
make_tree_walk_test!(scope);
make_tree_walk_test!(if_true);
make_tree_walk_test!(if_false);
make_tree_walk_test!(basic_logic);
make_tree_walk_test!(basic_while);
make_tree_walk_test!(for_loop);
make_tree_walk_test!(builtin_call, |output: &String| {
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
make_tree_walk_test!(basic_function_call);
make_tree_walk_test!(return_stmt_basic);
make_tree_walk_test!(return_stmt, ignore);
make_tree_walk_test!(closures, ignore);
make_tree_walk_test!(scope_update);
make_tree_walk_test!(resolving_and_binding);
