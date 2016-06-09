-module('qutil').
-export([valid_move/3, valid_wall/2, add_wall/3, wall_nodes/1, neighbors/1, reachable_neighbors/2, print_board/1, render_board/1, min_path/3]).

-record(board,
 {
   graph,
   positions,
   walls,
   existing,
   moves,
   cell_walls
 }).

valid_move(B, From, To) ->
    M = [N || {N, {1, _Path}} <- dijkstra:run(B#board.graph, From), N == To],
    case M of
	[To] ->
	    true;
	[] ->
	    false
    end.

% TODO: test that both players have clear paths remaining after wall
%       does a path exists to victory that doesn't traverse either of DelEdges?
%       want to be able to test without actually removing the edges
valid_wall(B, Wall) ->
    NW = string:left(Wall, 2),
    Orient = string:right(Wall, 1),
    Nodes = wall_nodes(NW),
    Conflict = [W || W <- B#board.existing, W =:= NW],
    case Conflict of
	[] ->
	    [_, SW, NE, SE] = Nodes,
	    Edges = graph:edges(B#board.graph),
	    case Orient of
		"h" ->
  		    % check for vertical edges, NW <-> SW and NE <-> SE
		    DelEdges = [E || E <- Edges, E == {NW, SW} orelse E == {SW, NW} orelse E == {NE, SE} orelse E == {SE, NE}];
		"v" ->
	            % check for horizontal edges, NW <-> NE and SW <-> SE
		    DelEdges = [E || E <- Edges, E == {NW, NE} orelse E == {NE, NW} orelse E == {SW, SE} orelse E == {SW, SE}]
	    end,
	    if length(DelEdges) =:= 2 ->
		    {valid, DelEdges};
	       true ->
		    {invalid, []}
	    end;
	_ ->
	    % invalid, crossing wall already exists
	    {invalid, []}
    end.

add_wall(B, Player, Wall) ->
    NW = string:left(Wall, 2),
    {Valid, Edges} = valid_wall(B, Wall),
    case Valid of
	valid ->
	    NewB = del_edges(B, Edges),
	    NewerB = annotate_cell_walls(NewB, Edges),
	    Walls = setelement(Player,
			       NewerB#board.walls,
			       element(Player, NewerB#board.walls) - 1),
	    % add NW wall node to Walls list for later invalidation
	    Existing = NewerB#board.existing,
	    {valid, NewerB#board{existing=[NW|Existing], walls=Walls}};
	invalid ->
	    {invalid, B}
    end.

del_edges(B, []) ->
    B;
del_edges(B, [{N1, N2}|Walls]) ->	    
    graph:del_edge(B#board.graph, {N1, N2}),
    graph:del_edge(B#board.graph, {N2, N1}),
    del_edges(B, Walls).

annotate_cell_walls(B, []) ->
    B;
annotate_cell_walls(B, Edges) ->
    [{N1, N2}|Rest] = Edges,
    [Lead|Tail] = lists:sort([N1, N2]),
    [Follow] = Tail,
    {_N, S, E, _W} = neighbors(Lead),
    if E == Follow ->
	    {Other, _} = maps:get(Lead, B#board.cell_walls, {nil, nil}),
	    NewMap = maps:put(Lead, {Other, "|"}, B#board.cell_walls),
	    annotate_cell_walls(B#board{cell_walls = NewMap}, Rest);
       S == Follow ->
	    {_, Other} = maps:get(Lead, B#board.cell_walls, {nil, nil}),
	    NewMap = maps:put(Lead, {"-", Other}, B#board.cell_walls),
	    annotate_cell_walls(B#board{cell_walls = NewMap}, Rest)
    end.

wall_nodes(NW) ->
    {_, SW, NE, _} = neighbors(NW),
    {_, SE, _, _} = neighbors(NE),
    [NW, SW, NE, SE].

% N, S, E, W cells wrt to Node; regardless of reachability
neighbors(Node) ->
    Col = string:left(Node, 1),
    {Row, _} = string:to_integer(string:right(Node, 1)),
    ColMask = "abcdefghi",
    ColIndex = string:str(ColMask, Col),
    % {N, S, E, W}
    case Row < 9 of
	true ->
	    S = Col ++ integer_to_list(Row + 1);
	false ->
	    S = nil
    end,
    case Row > 1 of
	true ->
	    N = Col ++ integer_to_list(Row - 1);
	false ->
	    N = nil
    end,
    case ColIndex < 9 of
	true ->
	    RightCol = string:substr(ColMask, ColIndex + 1, 1),
	    E = RightCol ++ integer_to_list(Row);
	false ->
	    E = nil
    end,
    case ColIndex > 1 of
	true ->
	    LeftCol = string:substr(ColMask, ColIndex - 1, 1),
	    W = LeftCol ++ integer_to_list(Row);
	false ->
	    W = nil
    end,
    {N, S, E, W}.

% reachable neighbors from Node, in no particular order
reachable_neighbors(B, Node) ->
    Edges = graph:edges(B#board.graph),
    lists:append([N || {N, C} <- Edges, C == Node], [N || {C, N} <- Edges, C == Node]).

print_board(B) ->
    Rendered = render_board(B),
    io:format("~ts", [Rendered]).

render_board(B) ->
    ColHeader = io_lib:format("  a b c d e f g h i~n", []),
    Board = lists:flatten([render_row(B, Idx) || Idx <- lists:seq(1, 9)]),
    io_lib:format("~ts~ts", [ColHeader, Board]).

render_row(B, Row) ->
    Mask = "abcdefghi",
    Sep =  lists:flatten([" ●" | [render_sep(B, Row,  string:substr(Mask, Idx, 1)) || Idx <- lists:seq(1, 9)]]),
    Text = lists:flatten([integer_to_list(Row) ++ "|" | [render_cell(B, Row, string:substr(Mask, Idx, 1)) || Idx <- lists:seq(1, 9)]]),
    case Row of
	1 ->
	    EmptySep = lists:flatten([" ●" | [render_sep(B, 0,  string:substr(Mask, Idx, 1)) || Idx <- lists:seq(1, 9)]]),
	    io_lib:format("~ts~n~ts~n~ts~n", [EmptySep, Text, Sep]);
	_ ->
	    io_lib:format("~ts~n~ts~n", [Text, Sep])
    end.    

render_cell(B, Row, Col) ->
    % colors
    %   red (player 2):  FF0000
    %   blue (player 1): 0000FF
    %   brown (walls):   422518
    Cell = Col ++ integer_to_list(Row),
    {_S, E} = maps:get(Cell, B#board.cell_walls, {nil, nil}),
    case E of
	nil ->
	    Pipe = "|";
	"|" ->
	    Pipe = color:true("422518", "‖")
    end,
    P1 = element(1, B#board.positions),
    P2 = element(2, B#board.positions),
    if P1 =:= Cell ->
	    [color:true("0000FF", "1"), Pipe];  % Blue
       P2 =:= Cell ->
	    [color:true("FF0000", "2"), Pipe];  % Red
       true ->
	    [" ", Pipe]                         % empty
    end.

render_sep(B, Row, Col) ->
    Cell = Col ++ integer_to_list(Row),
    {S, _E} = maps:get(Cell, B#board.cell_walls, {nil, nil}),
    case S of
	nil ->
	    Dash = "-";
	"-" ->
	    Dash = color:true("422518", "=")   % Brown
    end,
    [Dash, "●"].

min_path(B, Pos, Dest) ->
    Paths = [{N, {Len, Path}} || {N, {Len, Path}} <- dijkstra:run(B#board.graph, Pos), string:substr(N, 2, 1) == integer_to_list(Dest)],
    [Best|_Tail] = lists:sort(fun({_,{L,_}}, {_,{R,_}}) -> L < R end, Paths),
    Best.
