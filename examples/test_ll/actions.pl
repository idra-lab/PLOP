% Move arm to position
action(move_arm_start(Robot_id, Xi, Yi, Xf, Yf),
  [available(Robot_id), at(Robot_id, Xi, Yi)],
  [moving(Robot_id)],
  [],
  [arm(Robot_id), point(Xi, Yi), point(Xf, Yf)],
  [del(available(Robot_id)), add(moving(Robot_id, Xf, Yf))]
).
action(move_arm_end(Robot_id, Xi, Yi, Xf, Yf),
  [moving(Robot_id, Xf, Yf)],
  [],
  [],
  [arm(Robot_id), point(Xi, Yi), point(Xf, Yf)],
  [del(moving(Robot_id, Xf, Yf)), add(available(Robot_id)), add(at(Robot_id, Xf, Yf))]
).

% Grip action
action(grip_start(Robot_id, Stretch),
  [available(Robot_id)],
  [holding(Robot_id, _)],
  [],
  [arm(Robot_id)],
  [del(available(Robot_id)), add(gripping(Robot_id, Stretch))]
).
action(grip_end(Robot_id, Stretch),
  [gripping(Robot_id, Stretch)],
  [],
  [],
  [arm(Robot_id)],
  [del(gripping(Robot_id, Stretch)), add(holding(Robot_id, Stretch))]
).

% Release action
action(release_start(Robot_id),
  [holding(Robot_id, _)],
  [],
  [],
  [arm(Robot_id)],
  [del(holding(Robot_id, _)), add(releasing(Robot_id))]
).
action(release_end(Robot_id),
  [releasing(Robot_id)],
  [],
  [],
  [arm(Robot_id)],
  [del(releasing(Robot_id)), add(available(Robot_id))]
).