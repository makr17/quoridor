-module('qutil').
-export([valid_move/3, valid_wall/3, add_wall/3, wall_nodes/1, neighbors/1]).

valid_move(B, From, To) ->
    M = [N || {N, {1, _Path}} <- dijkstra:run(B, From), N == To],
    case M of
	[To] ->
	    true;
	[] ->
	    false
    end.

% TODO: need to check for crossing walls
%       e.g. e3h and e3v form an impossible "plus"
%       naive search of remaining edges doesn't catch this
valid_wall(B, Walls, Wall) ->
    NW = string:left(Wall, 2),
    Orient = string:right(Wall, 1),
    Nodes = wall_nodes(NW),
    SortedNodes = lists:sort(Nodes),
    Conflict = [W || W <- Walls, W == SortedNodes],
    case Conflict of
	[] ->
	    io:format("Wall ~s affects cells ~p~n", [Wall, Nodes]),
	    [_, SW, NE, SE] = Nodes,
	    Edges = graph:edges(B),
	    case Orient of
		"h" ->
  		    % check for vertical edges, NW <-> SW and NE <-> SE
		    DelEdges = [E || E <- Edges, E == {NW, SW} orelse E == {SW, NW} orelse E == {NE, SE} orelse E == {SE, NE}];
		"v" ->
	            % check for horizontal edges, NW <-> NE and SW <-> SE
		    DelEdges = [E || E <- Edges, E == {NW, NE} orelse E == {NE, NW} orelse E == {SW, SE} orelse E == {SW, SE}]
	    end,
	    io:format("~p wall bisects existing edges ~p~n", [Wall, DelEdges]);
	_ ->
	    % invalid, crossing wall already exists
	    DelEdges = []
    end,
    case length(DelEdges) == 2 of
	true ->
	    {valid, DelEdges};
	false ->
	    {invalid, DelEdges}
    end.

add_wall(B, Walls, Wall) ->
    NW = string:left(Wall, 2),
    {Valid, Edges} = valid_wall(B, Walls, Wall),
    case Valid of
	valid ->
	    io:format("removing edges ~p~n", [Edges]),
	    [del_edges(B, E) || E <- Edges],
	    Nodes = lists:sort(wall_nodes(NW)),
	    % add wall nodes to Walls list for later invalidation
	    {valid, B, [Nodes|Walls]};
	invalid ->
	    {invalid, B}
    end.

del_edges(B, {N1, N2}) ->	    
    graph:del_edge(B, {N1, N2}),
    graph:del_edge(B, {N2, N1}).

wall_nodes(NW) ->
    {_, SW, NE, _} = neighbors(NW),
    {_, SE, _, _} = neighbors(NE),
    [NW, SW, NE, SE].

neighbors(Node) ->
    Col = string:left(Node, 1),
    {Row, _} = string:to_integer(string:right(Node, 1)),
    ColMask = "abcdefghi",
    ColIndex = string:str(ColMask, Col),
    % {N, S, E, W}
    case Row < 9 of
	true ->
	    N = Col ++ integer_to_list(Row + 1);
	false ->
	    N = nil
    end,
    case Row > 1 of
	true ->
	    S = Col ++ integer_to_list(Row - 1);
	false ->
	    S = nil
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
