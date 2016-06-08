-module('qserver').
-behaviour('gen_server').
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([board/0]).

-record(board,
 {
   graph,
   positions,
   walls,
   existing,
   moves,
   cell_walls
 }).

% external interface

% gen_server functions
init([]) ->
    {ok, board()}.

% TODO: handle opponent adjacency
%       remove edge to opponent node
%       add edges from new node to opponent adjacent nodes
%       cache del/add so we can reverse once move has been made
handle_call({Player, move, Move}, _From, B) ->
    Pos = array:get(Player, B#board.positions),
    case qutil:valid_move(B, Pos, Move) of
	true ->
	    Moves = B#board.moves,
	    Positions = array:set(Player, Move, B#board.positions),
	    NewB = B#board{moves = [Move|Moves], positions = Positions},
	    {reply, valid, NewB};
	false ->
	    {reply, invalid, B}
    end;
handle_call({Player, wall, Wall}, _From, B) ->
    {Valid, NewB} = qutil:valid_wall(B, Wall),
    Count = array:get(Player, B#board.walls),
    case Count of
	0 ->
	    {reply, no_walls, B};
	_ ->
	    case Valid of
		valid ->
		    Walls = array:set(Player, Count - 1, NewB#board.walls),
		    Moves = NewB#board.moves,
		    {reply, valid, NewB#board{walls = Walls, moves = [Wall|Moves]}};
		invalid ->
		    {reply, invalid, B}
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
    #board{graph=graph:from_file(File, ReadVertices, ReadEdge),
	   positions = array:from_list(["e9", "e1"]),
	   walls = array:from_list([10, 10]),
	   existing=[],  % TODO: something more efficient
	   moves=[],
	   cell_walls=maps:new()
	  }.
    

