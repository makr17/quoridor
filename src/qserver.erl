-module('qserver').
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0, subscribe/1, board/1, move/2, done/1]).

-include("board.hrl").

% external interface

% gen_server functions
init([]) ->
    {ok, new_board()}.

% TODO: handle opponent adjacency
%       remove edge to opponent node
%       add edges from new node to opponent adjacent nodes
%       cache del/add so we can reverse once move has been made
handle_call({Player, Move}, _From, B) when length(Move) == 2 ->
    Pos = element(Player, B#board.positions),
    Valid = qutil:valid_move(B, Pos, Move),
    case Valid of
	true ->
	    Moves = B#board.moves,
	    Positions = setelement(Player, B#board.positions, Move),
	    NewB = B#board{moves = [Move|Moves], positions = Positions},
	    notify_subscribers(Player, Move, NewB),
	    {reply, {valid, NewB}, NewB};
	false ->
	    {reply, {invalid, B}, B}
    end;
handle_call({Player, Wall}, _From, B) when length(Wall) == 3 ->
    case element(Player, B#board.walls) of
	0 ->
	    {reply, {no_walls, B}, B};
	_ ->
	    case qutil:add_wall(B, Player, Wall) of
		{valid, NewB} ->
		    NewerB = NewB#board{moves = [Wall|NewB#board.moves]},
		    notify_subscribers(Player, Wall, NewerB),
		    {reply, {valid, NewerB}, NewerB};
		{invalid, NewB} ->
		    {reply, {invalid, NewB}, NewB}
	    end
    end;
handle_call(board, _From, B) ->
    {reply, {valid, B}, B};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State}.

handle_cast({subscribe, From}, B) ->
    Subs = B#board.subscribers,
    {noreply, B#board{subscribers = [From|Subs]}};
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

% external functions    
start_link() ->
    gen_server:start_link(?MODULE, [], []).

subscribe(Pid) ->
    gen_server:cast(Pid, {subscribe, self()}).

move(Pid, Move) ->
    gen_server:call(Pid, Move).

board(Pid) ->
    gen_server:call(Pid, board).

done(Pid) ->
    gen_server:call(Pid, terminate).

% internal functions
% read empty board graph from file
new_board() ->
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
    #board{graph=graph:from_file(File, ReadVertices, ReadEdge),
	   positions = {"e9", "e1"},
	   walls = {10, 10},
	   existing=[],  % TODO: something more efficient
	   moves=[],
	   cell_walls=maps:new(),
	   subscribers=[]
	  }.

notify_subscribers(Player, Move, B) ->
    Subs = B#board.subscribers,
    notify_subscribers(Player, Move, B, Subs).

notify_subscribers(_, _, B, []) ->
    B;
notify_subscribers(Player, Move, B, [Sub|_Tail]) ->
    qplayer:update(Sub, B, Player, Move).
