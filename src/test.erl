-module('test').

-export([all/0]).

all() ->
    all_tests().

all_tests() ->
    B = qserver:board(),
    test_qutil(B).

test_qutil(B) ->
    test_render(B),
    B1 = test_valid_walls(B),
    _B2 = test_add_walls(B1).

test_render(B) ->
    qutil:print_board(B).

test_valid_walls(B) ->
    Valid = ["a1h", "a1v", "a8h", "a8v", "h1h", "h1v", "h8h", "h8v", "e5h", "e5v"],
    B1 = test_valid_walls(B, valid, Valid),
    %Invalid = ["a9h", "a9v", "i9h"],
    Invalid = ["a9h", "a9v"],
    B2 = test_valid_walls(B1, invalid, Invalid),
    B2.

test_valid_walls(B, _Validity, []) ->
    B;
test_valid_walls(B, Validity, [Wall|Walls]) ->
    io:format("testing ~p wall ~s~n", [Validity, Wall]),
    {Validity, _} = qutil:valid_wall(B, Wall),
    test_valid_walls(B, Validity, Walls).


test_add_walls(B) ->
    Valid = ["e5h", "a1h", "c4v", "b1v"],
    B1 = test_add_walls(B, valid, Valid),
    Invalid = ["e5h", "e5v"],
    B2 = test_add_walls(B1, invalid, Invalid),
    B2.

test_add_walls(B, _Validity, []) ->
    B;
test_add_walls(B, Validity, [Wall|Walls]) ->
    io:format("adding ~p wall ~s~n", [Validity, Wall]),
    % player 1...  I suppose I should test with player 2 as well
    {Validity, B1} = qutil:add_wall(B, 1, Wall),
    test_render(B1),
    test_add_walls(B1, Validity, Walls).
