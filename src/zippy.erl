-module(zippy).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0, configure/3, start_play/2, pick_move/3, done/1]).

-include("board.hrl").

-define(wallp, "0.3").

-record(state,
 {
   server_pid,
   player,
   last_position
 }).

% gen_server requirements

init([]) ->
    {ok, #state{}}.

handle_call({configure, ServerPid, Player}, _From, S) ->
    qserver:subscribe(ServerPid),
    {reply, ok, S#state{server_pid = ServerPid, player = Player}};
handle_call({pick_move, Player, B, Bad}, _From, S) ->
    Move = pick_move(B, Player, Bad),
    {reply, {ok, Move}, S};
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

handle_cast({move, B, Player, _Move}, S) ->
    if Player =/= S#state.player ->
	    % other player moved, so now my turn
	    qplayer:make_move(
	      S#state.server_pid,
	      self(),
	      S#state.player,
	      B
	     ),
	    {noreply, S};
       true ->
	    % should be notification of the move I just made, ignore
	    {noreply, S}
    end;
handle_cast(Msg, S) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, S}.

handle_info(Msg, S) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, S}.

terminate(normal, _) ->
    ok.

code_change(_OldV, S, _Extra) ->
    {ok, S}.

% external functions

start_link() ->
    gen_server:start_link(?MODULE, [], []).

done(Pid) ->
    gen_server:call(Pid, terminate).

configure(Pid, ServerPid, Player) ->
    qplayer:configure(Pid, ServerPid, Player).

start_play(ServerPid, PlayerPid) ->
    qplayer:start_play(ServerPid, PlayerPid).

% internal functions

% TODO: should this be a call, so runs in zippy's process?
pick_move(B, Player, Bad) ->
    WallCount = element(Player, B#board.walls),
    % chance to choose wall decreases as walls are used
    % goes to zero when no more walls
    Ceiling = WallCount * strings:to_integer(?wallp),
    Rand = rand:uniform(),
    if Rand < Ceiling ->
	    Walls = valid(qplayer:valid_walls(B), Bad),
	    pick(Walls);
       true ->
	    Moves = valid(qplayer:valid_moves(B), Bad),
	    pick(Moves)
    end.

pick(List) ->
    lists:nth(random:uniform(length(List)), List).

% filter out moves that have been identified as bad
valid(List, Bad) ->
    [X || X <- List, not sets:is_element(X, Bad)].
