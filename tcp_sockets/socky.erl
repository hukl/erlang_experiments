-module(socky).

-behaviour(gen_server).

-export([ send_msg/1 ]).

-export([
    start_link/0,
    init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    code_change/3,
    terminate/2
  ]).
  
-record(state, { socket }).

send_msg( Message ) -> 
  gen_server:call(?MODULE, Message).

start_link() ->
  gen_server:start_link(?MODULE, [], []).
  
init([]) ->
  {ok, Socket} = gen_tcp:connect(
    {127,0,0,1}, 8081, [binary, {packet, 0}, {active, once}]
  ),
  gen_tcp:send(Socket, <<"Hello World">>),
  {ok, #state{ socket = Socket }}.
  
handle_call( Message, _From, State ) ->
  gen_tcp:send(State#state.socket, Message ),
  { reply, ok, State }.
  
handle_cast( _One, State ) ->
  { noreply, ok, State}.
  
handle_info( Msg, State ) ->
  io:format("GOT: ~p~n",[Msg]),
  inet:setopts(State#state.socket, [{ active, once }]),
  {noreply, State}.
  
code_change( _One, _Two, _Three ) ->
  ok.
  
terminate( _One, _Two ) ->
  ok.