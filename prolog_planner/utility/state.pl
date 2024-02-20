%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%                              STATE FUNCTIONS                               %%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

:- ensure_loaded('adts.pl').

change_state(S, [], S).
change_state(S, [add(P)|T], S_new) :-	
	change_state(S, T, S2),
	add_to_set(P, S2, S_new),
	!.
change_state(S, [del(P)|T], S_new) :-	
    change_state(S, T, S2),
	remove_from_set(P, S2, S_new),
	!.

member_state(S, [H|_]) :- 	equal_set(S, H).
member_state(S, [_|T]) :- 	member_state(S, T).

