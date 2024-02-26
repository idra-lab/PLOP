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

% Move robot to position
ll_action(move_base_start(Robot_id, Xi, Yi, Xf, Yf),
  [available(Robot_id), at(Robot_id, Xi, Yi)],
  [],
  [],
  [wheeled(Robot_id), point(Xi, Yi), point(Xf, Yf)],
  [del(available(Robot_id)), add(moving(Robot_id, Xf, Yf))]
).
ll_action(move_base_end(Robot_id, Xi, Yi, Xf, Yf),
  [moving(Robot_id, Xf, Yf)],
  [],
  [],
  [],
  [del(moving(Robot_id, Xf, Yf)), add(available(Robot_id)), add(at(Robot_id, Xf, Yf))]
).

% Move arm to position
ll_action(move_arm_start(Robot_id, Xi, Yi, Block),
  [available(Robot_id), at(Robot_id, Xi, Yi)],
  [moving(Robot_id)],
  [],
  [arm(Robot_id), point(Xi, Yi), block(Block, Xf, Yf, Zf)],
  [del(available(Robot_id)), add(moving(Robot_id, Xf, Yf, Zf))]
).
ll_action(move_arm_end(Robot_id, Xi, Yi, Xf, Yf, Zf),
  [moving(Robot_id, Xf, Yf)],
  [],
  [],
  [arm(Robot_id), point(Xi, Yi), point(Xf, Yf)],
  [del(moving(Robot_id, Xf, Yf)), add(available(Robot_id)), add(at(Robot_id, Xf, Yf))]
).

% Grip action
ll_action(grip_start(Robot_id, Block),
  [available(Robot_id)],
  [holding(Robot_id, _)],
  [],
  [arm(Robot_id), block(Block)],
  [del(available(Robot_id)), add(gripping(Robot_id, Block))]
).
ll_action(grip_end(Robot_id, Block),
  [gripping(Robot_id, Block)],
  [],
  [],
  [arm(Robot_id)],
  [del(gripping(Robot_id, Block)), add(holding(Robot_id, Block))]
).

% Release action
ll_action(release_start(Robot_id),
  [holding(Robot_id, _)],
  [],
  [],
  [arm(Robot_id)],
  [del(holding(Robot_id, _)), add(releasing(Robot_id))]
).
ll_action(release_end(Robot_id),
  [releasing(Robot_id)],
  [],
  [],
  [arm(Robot_id)],
  [del(releasing(Robot_id)), add(available(Robot_id))]
).