%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    HIGH-LEVEL ACTIONS                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(order_start(Client, Coffee, To_go),
    [available(barista), requested(Client, Coffee, To_go)],
    [],
    [],
    [coffee(Coffee)],
    [
        del(available(barista)), del(requested(Client, Coffee, To_go)),
        add(ordering(Client, Coffee, To_go))
    ]
).
action(order_end(Client, Coffee, To_go),
    [ordering(Client, Coffee, To_go)],
    [],
    [],
    [],
    [del(ordering(Client, Coffee, To_go)), add(ordered(Client, Coffee, To_go))]
).
action(payment_start(Client, Coffee, To_go),
    [ordered(Client, Coffee, To_go)],
    [],
    [],
    [],
    [del(ordered(Client, Coffee, To_go)), add(paying(Client, Coffee, To_go))]
).
action(payment_end(Client, Coffee, To_go),
    [paying(Client, Coffee, To_go)],
    [],
    [],
    [],
    [del(paying(Client, Coffee, To_go)), add(paid(Client, Coffee, To_go))]
).
action(make_coffee_start(Client, Coffee, To_go),
    [paid(Client, Coffee, To_go)],
    [],
    [],
    [],
    [del(paid(Client, Coffee, To_go)), add(making(Client, Coffee, To_go))]
).
action(make_coffee_end(Client, Coffee, To_go),
    [making(Client, Coffee, To_go)],
    [],
    [],
    [],
    [del(making(Client, Coffee, To_go)), add(made(Client, Coffee, To_go))]
).
action(serve_start(Client, Coffee, To_go),
    [made(Client, Coffee, To_go)],
    [],
    [],
    [],
    [del(made(Client, Coffee, To_go)), add(serving(Client, Coffee, To_go))]
).
action(serve_end(Client, Coffee, To_go),
    [serving(Client, Coffee, To_go)],
    [],
    [],
    [],
    [
        del(serving(Client, Coffee, To_go)), 
        add(served(Client, Coffee, To_go)), add(available(barista))
    ]
).
action(wash_start(Client, Coffee),
    [available(barista), served(Client, Coffee, not_to_go)],
    [],
    [],
    [],
    [add(washing(cup(Client, Coffee)))]
).
action(wash_end(Client, Coffee),
    [washing(cup(Client, Coffee))],
    [],
    [],
    [],
    [del(washing(cup(Client, Coffee))), add(washed(cup(Client, Coffee)))]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                      LOW-LEVEL ACTIONS                     %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

ll_action(move_base_start(Rob, Pos1, Pos2),
    [available(Rob), at(Rob, Pos1)],
    [at(_, Pos2)],
    [],
    [wheeled(Rob), pos(Pos1), pos(Pos2)],
    [del(at(Rob, Pos1)), del(available(Rob)), add(moving_base(Rob, Pos1, Pos2))]
).
ll_action(move_base_end(Rob, Pos1, Pos2),
    [moving_base(Rob, Pos1, Pos2)],
    [at(_, Pos2)],
    [],
    [],
    [del(moving_base(Rob, Pos1, Pos2)), add(at(Rob, Pos2)), add(available(Rob))]
).
ll_action(move_arm_start(Rob, X, Y, Z),
    [available(Rob)],
    [],
    [],
    [arm(Rob)],
    [del(available(Rob)), add(moving_arm(Rob, X, Y, Z))]
).
ll_action(move_arm_end(Rob, X, Y, Z),
    [moving_arm(Rob, X, Y, Z)],
    [],
    [],
    [],
    [del(moving_arm(Rob, X, Y, Z))]
).
ll_action(grip_start(Rob, Obj),
    [available(Rob)],
    [gripping(Rob, _), gripping(_, Obj), gripped(_, Obj), gripped(Rob, _)],
    [],
    [object(Obj), arm(Rob)],
    [del(available(Rob)), add(gripping(Rob, Obj))]
).
ll_action(grip_end(Rob, Obj),
    [gripping(Rob, Obj)],
    [],
    [],
    [],
    [del(gripping(Rob, Obj)), add(available(Rob)), add(gripped(Rob, Obj))]
).
ll_action(release_start(Rob, Obj),
    [available(Rob), gripped(Rob, Obj)],
    [],
    [],
    [],
    [del(available(Rob)), add(releasing(Rob, Obj))] % If I add del(gripped(Rob, Obj)) here, it may happen that the grip_start ll_action is
).
ll_action(release_end(Rob, Obj),
    [releasing(Rob, Obj)],
    [],
    [],
    [],
    [del(gripped(Rob, Obj)), add(available(Rob))]
).
ll_action(activate_machine_start(),
    [],
    [active_machine],
    [],
    [],
    [add(active_machine)]
).
ll_action(activate_machine_end(),
    [active_machine],
    [],
    [],
    [],
    [del(active_machine)]
).
ll_action(activate_register_start(),
    [],
    [active_register],
    [],
    [],
    [add(active_register)]
).
ll_action(activate_register_end(),
    [active_register],
    [],
    [],
    [],
    [del(active_register)]
).
ll_action(wait_start(Time),
    [],
    [waiting(_)],
    [],
    [],
    [add(waiting(Time))]
).
ll_action(wait_end(Time),
    [waiting(_)],
    [],
    [],
    [],
    [del(waiting(Time))]
).