testCase(Actions, Times) :- 
    go(
        [available(r1)],
        [available(r1), pillar(point(a)), pillar(point(b)), placed_architrave],
        Actions, 
        Times
    ).

testCase1(Actions, Times) :- 
    go(
        [available(r1)],
        [available(r1), pillar(point(a)), pillar(point(b)), placed_architrave],
        Actions, 
        Times
    ).

test(Action, Times) :- testCase(Action, Times). 
testNoTrace :- test(_A, _T). 
testTrace :- leash(-all), trace, testNoTrace. 

testSmallTrace :- 
	trace(action, all),
	trace(conditions_met, all), 
	trace(conditions_not_met, all), 
	trace(partial_order, all),
	trace(achiever, all),
	trace(plan, all),
	trace(stack, all),
	trace(testPlan, all),
	trace(stack, all),
	test(_A, _T).

testNew :-
	trace(action, all), 
	trace(partial_order, all), 
	trace(conditions_met, all), 
	test7(_A, _T).

