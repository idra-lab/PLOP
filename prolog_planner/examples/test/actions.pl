action(build_pillar_start(Robot_id, Point),
    [available(Robot_id)],
    [pillar(point(Point))],
    [],
    [arm(Robot_id), point(Point)],
    [
        del(available(Robot_id)), 
        add(building_pillar(Robot_id, Point))
    ]
).
action(build_pillar_end(Robot_id, Point),
    [building_pillar(Robot_id, Point)],
    [],
    [],
    [arm(Robot_id), point(Point)],
    [
        del(building_pillar(Robot_id, Point)),
        add(available(Robot_id)), add(pillar(point(Point)))
    ]
).

action(place_architrave_start(Robot_id),
    [available(Robot_id), pillar(point(Point1)), pillar(point(Point2))],
    [],
    [],
    [arm(Robot_id), point(Point1), point(Point2)],
    [
        del(available(Robot_id)),
        add(placing_architrave)
    ]
).
action(place_architrave_end(Robot_id),
    [placing_architrave],
    [],
    [],
    [arm(Robot_id)],
    [
        del(placing_architrave),
        add(available(Robot_id)), add(placed_architrave)
    ]
).