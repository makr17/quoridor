-module('qserver').
-behaviour('gen_server').
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([board/0]).

% external interface

% gen_server functions
init([]) ->
    B = board(),
    P1 = "e9",
    P2 = "e1",
    WallCounts = {8, 8},
    ExistingWalls = [], % TODO: something more efficient than a list?
    Moves = [],
    {ok, {{P1, P2}, B, WallCounts, ExistingWalls, Moves}}.

handle_call({Player, move, Move}, _From, State) ->
    {{P1, P2}, B, WallCounts, ExistingWalls, Moves} = State,
    case Player of
	"P1" ->
	    case qutil:valid_move(B, P1, Move) of
		true ->
		    {reply, valid, {{Move, P2}, B, WallCounts, ExistingWalls, [Move|Moves]}};
		false ->
		    {reply, invalid, State}
	    end;
	"P2" ->
	    case qutil:valid_move(B, P2, Move) of
		true ->
		    {reply, valid, {{P1, Move}, B, WallCounts, ExistingWalls, [Move|Moves]}};
		false ->
		    {reply, invalid, State}
	    end	
    end;
handle_call({Player, wall, Wall}, _From, State) ->
    {Players, B, {P1w, P2w}, ExistingWalls, Moves} = State,
    {Valid, NewB} = qutil:valid_wall(B, ExistingWalls, Wall),
    case Player of
	"P1" ->
	    if
		P1w =:= 0 ->
		    {reply, no_walls, State};
		Valid =:= valid ->
		    {reply, valid, {Players, NewB, {P1w - 1, P2w}, ExistingWalls, [Moves|Wall]}};
		true ->
		    {reply, invalid, State}
	    end;
	"P2" ->
	    if
		P2w =:= 0 ->
		    {reply, no_walls, State};
		Valid ->
		    {reply, valid, {Players, NewB, {P1w, P2w - 1}, ExistingWalls, [Moves|Wall]}};
		true ->
		    {reply, invalid, State}
	    end
    end;
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

handle_info(Msg, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

terminate(normal, _State) ->
    ok.

code_change(_OldV, State, _Extra) ->
    {ok, State}.
    

% internal functions
% read empty board graph from file
board() ->
    File = ?Q_DATA ++ "/empty_board.txt",
    ReadVertices =
	fun(IO, _N) -> 
		Ln = io:get_line(IO, ">"),
		string:tokens(string:strip(Ln, right, $\n), " ")
	end,
    %% Function to read each edge
    ReadEdge =
	fun(IO, _WT) ->
		{ok, [V1, V2, W]} = io:fread(IO, ">", "~s ~s ~d"),
		{V1, V2, W}
	end,
    graph:from_file(File, ReadVertices, ReadEdge).

