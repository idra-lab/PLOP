mappings(
    order_start(Client, Coffee, To_go), 
    [
        move_base_start(Rob, Pos1, service_area), 
        move_base_end(Rob, Pos1, service_area), 
        queue_order_start(),
        queue_order_end()
    ]).

mappings(
    payment_start(Client, Coffee, To_go), 
    [
        move_base_start(Rob, Pos1, register),
        move_base_end(Rob, Pos1, register),
        activate_register_start(),
        activate_register_end()
    ]).

mappings(
    make_coffee_start(Client, espresso, to_go), 
    [
        % Go take the cup
        move_base_start(Rob, Pos1, cup_dispender),
        move_base_end(Rob, Pos1, cup_dispender),
        move_arm_end(Rob, 4.75, -1, 5),
        move_arm_end(Rob, 4.75, -1, 5),
        grip_start(Rob, paper_cup),
        grip_end(Rob, paper_cup),
        % Move to coffe machine and release cup
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup),
        % Take the coffee chamber and fill it with coffee
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        grip_start(Rob, coffee_chamber),
        grip_end(Rob, coffee_chamber),
        move_base_start(Rob, coffee_machine, ingredient_area),
        move_base_end(Rob, coffee_machine, ingredient_area),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        wait_start(5),
        wait_end(5),
        % Move to the coffee machine and release the coffee chamber
        move_base_start(Rob, ingredient_area, coffee_machine),
        move_base_end(Rob, ingredient_area, coffee_machine),
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        release_start(Rob, coffee_chamber),
        release_end(Rob, coffee_chamber),
        % Activate the coffee machine and wait for 20 seconds for the coffee to be ready
        activate_coffee_machine_start(),
        activate_coffee_machine_end(),
        wait_start(20),
        wait_end(20)
    ]).

mappings(
    make_coffee_start(Client, espresso, not_to_go), 
    [
        % Go take the cup
        move_base_start(Rob, Pos1, cup_dispender),
        move_base_end(Rob, Pos1, cup_dispender),
        move_arm_end(Rob, 4.25, -1, 5),
        move_arm_end(Rob, 4.25, -1, 5),
        grip_start(Rob, china_cup),
        grip_end(Rob, china_cup),
        % Move to coffe machine and release cup
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        release_start(Rob, china_cup),
        release_end(Rob, china_cup),
        % Take the coffee chamber and fill it with coffee
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        grip_start(Rob, coffee_chamber),
        grip_end(Rob, coffee_chamber),
        move_base_start(Rob, coffee_machine, ingredient_area),
        move_base_end(Rob, coffee_machine, ingredient_area),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        wait_start(5),
        wait_end(5),
        % Move to the coffee machine and release the coffee chamber
        move_base_start(Rob, ingredient_area, coffee_machine),
        move_base_end(Rob, ingredient_area, coffee_machine),
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        release_start(Rob, coffee_chamber),
        release_end(Rob, coffee_chamber),
        % Activate the coffee machine and wait for 20 seconds for the coffee to be ready
        activate_coffee_machine_start(),
        activate_coffee_machine_end(),
        wait_start(20),
        wait_end(20)
    ]).

mappings(
    make_coffee_start(Client, cappuccino, to_go), 
    [
        % Go take the cup
        move_base_start(Rob, Pos1, cup_dispender),
        move_base_end(Rob, Pos1, cup_dispender),
        move_arm_end(Rob, 4.75, -1, 5),
        move_arm_end(Rob, 4.75, -1, 5),
        grip_start(Rob, paper_cup),
        grip_end(Rob, paper_cup),
        % Move to coffe machine and release cup
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup),
        % Take the coffee chamber and fill it with coffee
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        grip_start(Rob, coffee_chamber),
        grip_end(Rob, coffee_chamber),
        move_base_start(Rob, coffee_machine, ingredient_area),
        move_base_end(Rob, coffee_machine, ingredient_area),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        wait_start(5),
        wait_end(5),
        % Move to the coffee machine and release the coffee chamber
        move_base_start(Rob, ingredient_area, coffee_machine),
        move_base_end(Rob, ingredient_area, coffee_machine),
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        release_start(Rob, coffee_chamber),
        release_end(Rob, coffee_chamber),
        % Activate the coffee machine and wait for 20 seconds for the coffee to be ready
        activate_coffee_machine_start(),
        activate_coffee_machine_end(),
        wait_start(20),
        wait_end(20),
        % Take the cup and fill it with milk
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        grip_start(Rob, paper_cup),
        grip_end(Rob, paper_cup),
        move_base_start(Rob, coffee_machine, milk_dispender),
        move_base_end(Rob, coffee_machine, milk_dispender),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup),
        wait_start(5),
        wait_end(5)
    ]).

mappings(
    make_coffee_start(Client, cappuccino, not_to_go), 
    [
        % Go take the cup
        move_base_start(Rob, Pos1, cup_dispender),
        move_base_end(Rob, Pos1, cup_dispender),
        move_arm_end(Rob, 4.25, -1, 5),
        move_arm_end(Rob, 4.25, -1, 5),
        grip_start(Rob, china_cup),
        grip_end(Rob, china_cup),
        % Move to coffe machine and release cup
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_base_start(Rob, cup_dispender, coffee_machine),
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        release_start(Rob, china_cup),
        release_end(Rob, china_cup),
        % Take the coffee chamber and fill it with coffee
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        grip_start(Rob, coffee_chamber),
        grip_end(Rob, coffee_chamber),
        move_base_start(Rob, coffee_machine, ingredient_area),
        move_base_end(Rob, coffee_machine, ingredient_area),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        wait_start(5),
        wait_end(5),
        % Move to the coffee machine and release the coffee chamber
        move_base_start(Rob, ingredient_area, coffee_machine),
        move_base_end(Rob, ingredient_area, coffee_machine),
        move_arm_start(Rob, 3, 1, 6),
        move_arm_end(Rob, 3, 1, 6),
        release_start(Rob, coffee_chamber),
        release_end(Rob, coffee_chamber),
        % Activate the coffee machine and wait for 20 seconds for the coffee to be ready
        activate_coffee_machine_start(),
        activate_coffee_machine_end(),
        wait_start(20),
        wait_end(20),
        % Take the cup and fill it with milk
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        grip_start(Rob, china_cup),
        grip_end(Rob, china_cup),
        move_base_start(Rob, coffee_machine, milk_dispender),
        move_base_end(Rob, coffee_machine, milk_dispender),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        release_start(Rob, china_cup),
        release_end(Rob, china_cup),
        wait_start(5),
        wait_end(5)

    ]).

mappings(
    serve_start(Client, espresso, to_go), 
    [
        % Take the cup up again
        move_base_start(Rob, Pos1, coffee_machine),
        move_base_end(Rob, Pos1, coffee_machine),
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        grip_start(Rob, paper_cup),
        grip_end(Rob, paper_cup),
        % Move to the service area and release the cup
        move_base_start(Rob, coffee_machine, service_area),
        move_base_end(Rob, coffee_machine, service_area),
        move_arm_start(Rob, 1, -1, 5),
        move_arm_end(Rob, 1, -1, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup)
    ]).

mappings(
    serve_start(Client, espresso, not_to_go), 
    [
        move_base_start(Rob, Pos1, coffee_machine),
        move_base_end(Rob, Pos1, coffee_machine),
        move_arm_start(Rob, 3, 1, 5),
        move_arm_end(Rob, 3, 1, 5),
        grip_start(Rob, china_cup),
        grip_end(Rob, china_cup),
        % Move to the service area and release the cup
        move_base_start(Rob, coffee_machine, service_area),
        move_base_end(Rob, coffee_machine, service_area),
        move_arm_start(Rob, 1, -1, 5),
        move_arm_end(Rob, 1, -1, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup)
    ]).

mappings(
    serve_start(Client, cappuccino, to_go), 
    [
        % Take the cup up again
        move_base_start(Rob, Pos1, coffee_machine),
        move_base_end(Rob, Pos1, coffee_machine),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        grip_start(Rob, paper_cup),
        grip_end(Rob, paper_cup),
        % Move to the service area and release the cup
        move_base_start(Rob, coffee_machine, service_area),
        move_base_end(Rob, coffee_machine, service_area),
        move_arm_start(Rob, 1, -1, 5),
        move_arm_end(Rob, 1, -1, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup)
    ]).

mappings(
    serve_start(Client, cappuccino, not_to_go), 
    [
        move_base_start(Rob, Pos1, coffee_machine),
        move_base_end(Rob, Pos1, coffee_machine),
        move_arm_start(Rob, 4.5, 2, 5),
        move_arm_end(Rob, 4.5, 2, 5),
        grip_start(Rob, china_cup),
        grip_end(Rob, china_cup),
        % Move to the service area and release the cup
        move_base_start(Rob, coffee_machine, service_area),
        move_base_end(Rob, coffee_machine, service_area),
        move_arm_start(Rob, 1, -1, 5),
        move_arm_end(Rob, 1, -1, 5),
        release_start(Rob, paper_cup),
        release_end(Rob, paper_cup)
    ]).

mappings(
    wash_start(Client, Coffee, not_to_go), 
    [
        move_base_start(Rob, Pos1, service_area),
        move_base_end(Rob, Pos1, service_area),
        move_arm_start(Rob, 1, -1, 5),
        move_arm_end(Rob, 1, -1, 5),
        grip_start(Rob, china_cup),
        grip_end(Rob, china_cup),
        move_base_start(Rob, service_area, sink),
        move_base_end(Rob, service_area, sink),
        move_arm_start(Rob, 0, 2, 3),
        move_arm_end(Rob, 0, 2, 3),
        release_start(Rob, china_cup),
        release_end(Rob, china_cup)
    ]).

