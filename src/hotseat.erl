-module('hotseat').

-export([start/0]).

start() ->
    {ok, Pid} = qserver:start_link(),
    input_loop(Pid, 1).

input_loop(Pid, P) ->
    case P of
	0 ->
	    Player = 2;
	1 ->
	    Player = 1;
	2 ->
	    Player = 2;
	_ ->
	    Player = 1  % TODO
    end,
    {valid, Board} = qserver:board(Pid),
    io:format("~ts~n", [qutil:render_board(Board)]),
    {Score, MeBest, OppBest} = qplayer:score_board(Board, Player),
    io:format("relative score: ~p~n", [Score]),
    {MeDest, {MeLen, _}} = MeBest,
    io:format("my best path is to ~s length ~p~n", [MeDest, MeLen]),
    {OppDest, {OppLen, _}} = OppBest,
    io:format("opponent best path is to ~s length ~p~n", [OppDest, OppLen]),
    {ok, Input} = io:fread("P" ++ integer_to_list(Player) ++ "> ", "~s"),
    [Move|_Tail] = Input,
    case Move of
	"quit" ->
	    qserver:done(Pid);
	_ ->
	    case qserver:move(Pid, {Player, Move}) of
		{valid, _B} ->
		    input_loop(Pid, (Player + 1) rem 2);
		{invalid, _B} ->
		    io:format("~s in an invalid move!~n", [Move]),
		    input_loop(Pid, Player)
	    end
    end.
	
