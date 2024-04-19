extract_resources_number(Resources) :-
  findall(X, resources(X), AllResources),
  extract_resources_number(AllResources, [], Resources).

extract_resources_number([], Resources, Resources).
extract_resources_number([Resource|T], TmpResources, Resources) :- 
  findall(Resource, Resource, List),
  length(List, Len),
  functor(Resource, ResourceName, _),
  append([ResourceName-Len], TmpResources, NewTmpResources),
  extract_resources_number(T, NewTmpResources, Resources).



extract_resources_per_action(TTActions, Resources, ActionXResources) :-
  [H|T] = TTActions,
  extract_resources_per_action(T, Resources, [], ActionXResources).

extract_resources_per_action([], _Resources, ActionXResources, ActionXResources).
extract_resources_per_action([TT_ID-StartAction-EndAction|T], Resources, TmpActionXResources, ActionXResources) :-
  check_resources_per_action(StartAction, EndAction, Resources, [], RetResources),
  append([TT_ID-RetResources], TmpActionXResources, NewActionXResources),
  extract_resources_per_action(T, Resources, NewActionXResources, ActionXResources).

check_resources_per_action(StartAction, EndAction, [], UsedResources, UsedResources).
check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is used in ~w~n', [Resource, StartAction]),
  append([Resource], UsedResources, NewUsedResources),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, NewUsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  ll_action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is used in ~w~n', [Resource, StartAction]),
  append([Resource], UsedResources, NewUsedResources),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, NewUsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  \+check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is NOT used in ~w~n', [Resource, StartAction]),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, UsedResources, RetResources),
  true.

check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], [Resource-_Q|T], UsedResources, RetResources) :-
  % format('Checking resources for ~w ~w ~w~n', [StartAction, EndAction, Resource]),
  ll_action(StartAction, _, _, _, Verify, _),
  % format('Verify ~w~n', [Verify]),
  \+check_resource_in_verify(Resource, Verify),
  % format('Resource ~w is NOT used in ~w~n', [Resource, StartAction]),
  check_resources_per_action([IDStart-StartAction], [IDEnd-EndAction], T, UsedResources, RetResources),
  true.

check_resource_in_verify(Resource, []) :- fail.
check_resource_in_verify(Resource, [H|T]) :-
  % format('Checking resource ~w in ~w~n', [Resource, H]),
  H =.. [Resource | _],
  true.
check_resource_in_verify(Resource, [H|T]) :-
  % format('Checking resource ~w in ~w~n', [Resource, H]),
  \+ (H =.. [Resource | _]),
  check_resource_in_verify(Resource, T).

