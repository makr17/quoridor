-module('qserver').
-behaviour('gen_server').
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([board/0]).

-record(board,
 {
  graph,
  p1,
  p2,
  p1walls,
  p2walls,
  existing,
  moves,
  cell_walls
 }).

% external interface

% gen_server functions
init([]) ->
    B = board(),
    {ok, B}.

% TODO: handle opponent adjacency
%       remove edge to opponent node
%       add edges from new node to opponent adjacent nodes
%       cache del/add so we can reverse once move has been made
handle_call({Player, move, Move}, _From, B) ->
    case Player of
	"P1" ->
	    case qutil:valid_move(B, B#board.p1, Move) of
		true ->
		    Moves = B#board.moves,
		    NewB = B#board{moves = [Move|Moves], p1 = Move},
		    {reply, valid, NewB};
		false ->
		    {reply, invalid, B}
	    end;
	"P2" ->
	    case qutil:valid_move(B, B#board.p2, Move) of
		true ->
		    Moves = B#board.moves,
		    NewB = B#board{moves = [Move|Moves], p2 = Move},
		    {reply, valid, NewB};
		false ->
		    {reply, invalid, B}
	    end	
    end;
handle_call({Player, wall, Wall}, _From, B) ->
    {Valid, NewB} = qutil:valid_wall(B, Wall),
    case Player of
	"P1" ->
	    if
		B#board.p1walls =:= 0 ->
		    {reply, no_walls, B};
		Valid =:= valid ->
		    P1w = NewB#board.p1walls - 1,
		    Moves = NewB#board.moves,
		    NewerB = NewB#board{p1walls = P1w, moves = [Wall|Moves]},
		    {reply, valid, NewerB};
		true ->
		    {reply, invalid, B}
	    end;
	"P2" ->
	    if
		B#board.p2walls =:= 0 ->
		    {reply, no_walls, B};
		Valid ->
		    P2w = NewB#board.p2walls - 1,
		    Moves = NewB#board.moves,
		    NewerB = NewB#board{p2walls = P2w, moves = [Wall|Moves]},
		    {reply, valid, NewerB};
		true ->
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
	   p1="e9",
	   p2="e1",
	   p1walls=10,
	   p2walls=10,
	   existing=[],  % TODO: something more efficient
	   moves=[],
	   cell_walls=maps:new()
	  }.
    

