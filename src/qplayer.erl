-module('qplayer').

-export([valid_moves/2, valid_walls/1, score_board/2]).

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

% a1h .. h1h .... a8h .. h8h
% a1v .. h1v .... a8v .. h8v
% don't need to test Col i or Row 9 since walls have length of two
valid_walls(B) ->
    valid_walls(B, lists:seq(1,8), ["a", "b", "c", "d", "e", "f", "g", "h"], []).

valid_walls(_B, [], [], Acc) ->
    Acc;
valid_walls(B, [Row|Rows], Cols, Acc) ->
    io:format("valid_walls: ~p~n", [Acc]),
    HAcc = valid_row_walls(B, Row, Cols, "h", Acc),
    VAcc = valid_row_walls(B, Row, Cols, "v", HAcc),
    valid_walls(B, Rows, Cols, VAcc).

valid_row_walls(_B, _, [], _, Acc) ->
    Acc;
valid_row_walls(B, Row, [Col|Cols], Orient, Acc) ->
    Wall = Col ++ integer_to_list(Row) ++ Orient,
    io:format("valid_row_walls: ~p~n", [Acc]),
    case qutil:valid_wall(B, Wall) of
	{valid, _} ->
	    valid_row_walls(B, Row, Cols, Orient, [Wall|Acc]);
	{invalid, _} ->
	    valid_row_walls(B, Row, Cols, Orient, Acc)
    end.

score_board(B, Player) ->
    % TODO: stash in board if this becomes a burden
    Dests = {1, 9},
    Me = element(Player, B#board.positions),
    [Opp] = [N || N <- tuple_to_list(B#board.positions), N =/= Me],
    Dest = element(Player, Dests),
    MeBest = qutil:min_path(B, Me, Dest),
    [OppDest] = [N || N <- tuple_to_list(Dests), N =/= Dest],
    OppBest = qutil:min_path(B, Opp, OppDest),
    {_, {MeLen, _}} = MeBest,
    {_, {OppLen, _}} = OppBest,
    % TODO: factor in value of remaining walls?
    {OppLen - MeLen, MeBest, OppBest}.
