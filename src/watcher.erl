-module(watcher).
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, code_change/3, terminate/2]).
-export([start_link/0, configure/3, done/1, start_play/2]).

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
handle_call(terminate, _From, State) ->
    {stop, normal, ok, State};
handle_call(Msg, _From, State) ->
    io:format("Unexpected message: ~p~n",[Msg]),
    {noreply, State}.

handle_cast({move, B, Player, Move}, S) ->
    io:format("~ts~n", [qutil:render_board(B)]),
    io:format("player ~p played ~s~n", [Player, Move]),
    {noreply, S};
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

% external interface

start_link() ->
    gen_server:start_link(?MODULE, [], []).

done(Pid) ->
    gen_server:call(Pid, terminate).

configure(Pid, ServerPid, Player) ->
    qplayer:configure(Pid, ServerPid, Player).

start_play(ServerPid, PlayerPid) ->
    qplayer:start_play(ServerPid, PlayerPid).
