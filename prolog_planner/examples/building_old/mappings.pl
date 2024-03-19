mappings(
    build_pillar_start(Robot_id, a),
    [
        move_arm_start(Robot_id, 1, 1, 0),
        move_arm_end(Robot_id, 1, 1, 0),
        grip_start(Robot_id),
        grip_end(Robot_id),
        move_arm_start(Robot_id, 5, 5, 0),
        move_arm_end(Robot_id, 5, 5, 0),
        release_start(Robot_id),
        release_end(Robot_id),
        move_arm_start(Robot_id, 1, 2, 0),
        move_arm_end(Robot_id, 1, 2, 0),
        grip_start(Robot_id),
        grip_end(Robot_id),
        move_arm_start(Robot_id, 5, 5, 1),
        move_arm_end(Robot_id, 5, 5, 1),
        release_start(Robot_id),
        release_end(Robot_id)
    ]
).

mappings(
    build_pillar_start(Robot_id, b),
    [
        move_arm_start(Robot_id, 2, 1, 0),
        move_arm_end(Robot_id, 2, 1, 0),
        grip_start(Robot_id),
        grip_end(Robot_id),
        move_arm_start(Robot_id, 3, 3, 0),
        move_arm_end(Robot_id, 3, 3, 0),
        release_start(Robot_id),
        release_end(Robot_id),
        move_arm_start(Robot_id, 2, 2, 0),
        move_arm_end(Robot_id, 2, 2, 0),
        grip_start(Robot_id),
        grip_end(Robot_id),
        move_arm_start(Robot_id, 3, 3, 1),
        move_arm_end(Robot_id, 3, 3, 1),
        release_start(Robot_id),
        release_end(Robot_id)
    ]
).

mappings(
    place_architrave_start(Robot_id, a, b),
    [
        move_arm_start(Robot_id, 9, 9, 0),
        move_arm_end(Robot_id, 9, 9, 0),
        grip_start(Robot_id),
        grip_end(Robot_id),
        move_arm_start(Robot_id, 4, 4, 0),
        move_arm_end(Robot_id, 4, 4, 0),
        release_start(Robot_id),
        release_end(Robot_id)
    ]
).