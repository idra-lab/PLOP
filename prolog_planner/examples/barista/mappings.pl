mappings(
    order_start(Client, Coffee, To_go), 
    [
        move_base_start(Rob, Pos1, Service_area), 
        move_base_end(Rob, Pos1, Service_area), 
        queue_order(Client, Coffee)
    ]).

mappings(
    payment_start(Client, Coffee, To_go), 
    [
        move_base_start(Rob, Pos1)
    ]).

mappings(
    make_coffee_start(Client, espresso, To_go), 
    [
        test1,
        test5
    ]).

mappings(
    make_coffee_start(Client, cappuccino, To_go), 
    [
        test2,
        test6
    ]).


mappings(
    serve_start(Client, Coffee, To_go), 
    [
        test3,
        test7
    ]).

mappings(
    wash_start(Client, Coffee, To_go), 
    [
        test4,
        test8
    ]).

