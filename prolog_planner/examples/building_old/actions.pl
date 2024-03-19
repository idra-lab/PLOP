%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                    HIGH-LEVEL ACTIONS                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

action(place_architrave_start(Robot_id, Point1, Point2),
    [available(Robot_id), pillar(Point1), pillar(Point2)],
    [placed_architrave(_, _), placing_architrave(_, _, _)],
    [],
    [arm(Robot_id), point(Point1), point(Point2), Point1 \= Point2],
    [
        del(available(Robot_id)),
        add(placing_architrave(Robot_id, Point1, Point2))
    ]
).
action(place_architrave_end(Robot_id, Point1, Point2),
    [placing_architrave(Robot_id, Point1, Point2)],
    [],
    [],
    [arm(Robot_id)],
    [
        del(placing_architrave(Robot_id, Point1, Point2)),
        add(available(Robot_id)), add(placed_architrave(Point1, Point2))
    ]
).

action(build_pillar_start(Robot_id, Point),
    [available(Robot_id)],
    [pillar(Point), building_pillar(_, Point)],
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
        add(available(Robot_id)), add(pillar(Point))
    ]
).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     INT-LEVEL ACTIONS                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% ll_action(move_block_start(Robot_id, Block, Xi, Yi, Zi, PointF, Xf, Yf, Zf),
%   [available(Robot_id), block(Block, PointI)],
%   [block(_, PointF), moving(_, Block, Xf, Yf, Zf)],
%   [],
%   [point(PointF, Xf, Yf, Zf), point(PointI, Xi, Yi, Zi)],
%   [
%     del(block(Block, PointI)), del(available(Robot_id)),
%     add(moving(Robot_id, Block, Xi, Yi, Zi, PointF, Xf, Yf, Zf))
%   ]
% ).

% ll_action(move_block_end(Robot_id, Block, Xi, Yi, Zi, PointF, Xf, Yf, Zf),
%   [moving(Robot_id, Block, Xi, Yi, Zi, PointF, Xf, Yf, Zf)],
%   [],
%   [],
%   [point(PointF, Xf, Yf, Zf), point(PointI, Xi, Yi, Zi)],
%   [
%     del(moving(Robot_id, Block, Xi, Yi, Zi, PointF, Xf, Yf, Zf))
%     add(block(Block, PointF)), add(available(Robot_id)),
%   ]
% ).

% ll_action(move_architrave_start(Robot_id, Point1, Point2, Xi, Yi, Zi, Xf, Yf, Zf),
%   [available(Robot_id), block(architrave, PointI)],
%   [moving_architrave(_, Point1, Point2, Xf, Yf, Zf)],
%   [],
%   [
%     point(Point1, X1, Y1, Z1), point(Point2, X2, Y2, Z2), 
%     Xf is (X2+X1)/2.0, Yf is (Y2+Y1)/2.0, 
%     point(architrave, Xa, Ya, Za), retract(point(architrave, Xa, Ya, Za)),
%     assertz(point(architrave, Xf, Yf, Zf))
%   ],
%   [
%     del(block(architrave, PointI)), del(available(Robot_id)),
%     add(moving_architrave(Robot_id, architrave, Xf, Yf, Zf))
%   ]
% ).

% ll_action(move_architrave_end(Robot_id, Point1, Point2, Xf, Yf, Zf),
%   [moving(Robot_id, architrave, Xf, Yf, Zf)],
%   [],
%   [],
%   [
%     point(PointF, Xf, Yf, Zf), point(PointI, _Xi, _Yi, _Zi)
%   ],
%   [
%     del(moving(Robot_id, architrave, Xf, Yf, Zf))
%     add(block(architrave, PointF)), add(available(Robot_id)),
%   ]
% ).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                     LOW-LEVEL ACTIONS                      %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

% Move arm to position
ll_action(move_arm_start(Robot_id, Xf, Yf, Zf),
  [available(Robot_id)],
  [],
  [],
  [arm(Robot_id)],
  [
    del(available(Robot_id)), 
    add(moving(Robot_id, Xf, Yf, Zf))
  ]
).
ll_action(move_arm_end(Robot_id, Xf, Yf, Zf),
  [moving(Robot_id, Xf, Yf, Zf)],
  [],
  [],
  [arm(Robot_id)],
  [
    del(moving(Robot_id, Xf, Yf, Zf)),
    add(available(Robot_id))
  ]
).

% Grip action
ll_action(grip_start(Robot_id),
  [available(Robot_id)],
  [holding(Robot_id), gripping(_)],
  [],
  [arm(Robot_id)],
  [
    del(available(Robot_id)), 
    add(gripping(Robot_id))
  ]
).
ll_action(grip_end(Robot_id),
  [gripping(Robot_id)],
  [],
  [],
  [arm(Robot_id)],
  [
    del(gripping(Robot_id)), 
    add(holding(Robot_id))
  ]
).

% Release action
ll_action(release_start(Robot_id),
  [available(Robot_id), holding(Robot_id)],
  [],
  [],
  [arm(Robot_id)],
  [
    del(holding(Robot_id)), 
    add(releasing(Robot_id))
  ]
).
ll_action(release_end(Robot_id),
  [releasing(Robot_id)],
  [],
  [],
  [arm(Robot_id)],
  [
    del(releasing(Robot_id)), 
    add(available(Robot_id))
  ]
).

% % Grip action
% ll_action(grip_start(Robot_id, Block),
%   [available(Robot_id)],
%   [holding(Robot_id, _), holding(_, Block), gripping(_, Block)],
%   [],
%   [arm(Robot_id), block(Block)],
%   [
%     del(available(Robot_id)), 
%     add(gripping(Robot_id, Block))
%   ]
% ).
% ll_action(grip_end(Robot_id, Block),
%   [gripping(Robot_id, Block)],
%   [],
%   [],
%   [arm(Robot_id)],
%   [
%     del(gripping(Robot_id, Block)), 
%     add(holding(Robot_id, Block))
%   ]
% ).

% % Release action
% ll_action(release_start(Robot_id, Block),
%   [available(Robot_id), holding(Robot_id, Block)],
%   [],
%   [],
%   [arm(Robot_id)],
%   [
%     del(holding(Robot_id, Block)), 
%     add(releasing(Robot_id, Block))
%   ]
% ).
% ll_action(release_end(Robot_id, Block),
%   [releasing(Robot_id, Block)],
%   [],
%   [],
%   [arm(Robot_id)],
%   [
%     del(releasing(Robot_id, Block)), 
%     add(available(Robot_id))
%   ]
% ).