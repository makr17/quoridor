-module('qplayer').

-export([valid_moves/2, valid_walls/1, score_board/2, min_path/3]).

-record(board,
 {
   graph,
   positions,
   walls,
   existing,
   moves,
   cell_walls
 }).

valid_moves(B, Player) ->
    Pos = element(Player, B#board.positions),
    qutil:reachable_neighbors(B, Pos).

valid_walls(B) ->
    B.

score_board(B, Player) ->
    % TODO: stash in board if this becomes a burden
    Dests = {1, 9},
    Me = element(Player, B#board.positions),
    [Opp] = [N || N <- tuple_to_list(B#board.positions), N =/= Me],
    Dest = element(Player, Dests),
    MeBest = min_path(B, Me, Dest),
    [OppDest] = [N || N <- tuple_to_list(Dests), N =/= Dest],
    OppBest = min_path(B, Opp, OppDest),
    {_, {MeLen, _}} = MeBest,
    {_, {OppLen, _}} = OppBest,
    % TODO: factor in value of remaining walls?
    MeLen - OppLen.

min_path(B, Pos, Dest) ->
    Paths = [{N, {Len, Path}} || {N, {Len, Path}} <- dijkstra:run(B#board.graph, Pos), string:substr(N, 2, 1) == integer_to_list(Dest)],
    [Best|_Tail] = lists:sort(fun({_,{L,_}}, {_,{R,_}}) -> L < R end, Paths),
    Best.
