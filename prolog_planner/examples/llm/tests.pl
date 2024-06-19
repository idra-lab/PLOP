hl_init([
  available(a1), available(a2), 
  onTable(b1, 2, 2), onTable(b2, 4, 4), onTable(b4, 8, 8), 
  on(b5, b1, 2, 2), on(b3, b2, 4, 4), 
  clear(b3), clear(b5), clear(b4)
]).

hl_goal([
  available(a1), available(a2), 
  onTable(b2, 4, 4), onTable(b4, 8, 8), onTable(b5, 5, 5),
  on(b1, b5, 5, 5), on(b3, b2, 4, 4), 
  clear(b1), clear(b3), clear(b4)
]).
