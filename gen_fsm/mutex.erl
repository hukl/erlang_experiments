-module( mutex ).


%% public API
-export([start/0, wait/0, signal/0]).

%% gen_fsm callbacks
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
    terminate/3, code_change/4]).

% States
-export([free/3, busy/3]).

-record( state, { clients=[] } ).

-behaviour( gen_fsm ).

init( [] ) ->
  process_flag(trap_exit, true),
  error_logger:info_msg("Initializing ~n"),
  {ok, free, #state{}}.

handle_event( _Event, _PreviousEvent, StateData ) ->
  {stop, bad_arg, StateData}.

handle_sync_event( _Event, _From, _StateName, StateData ) ->
  error_logger:info_msg("StateData ~p ~n", [StateData]).

handle_info( Info, StateName, StateData ) ->
  error_logger:info_msg("Info: ~p ~n", [Info]),

  { 'EXIT', Pid, Reason } = Info,

  % Check if deceased client was even in our client list
  case proplists:get_value( Pid, StateData#state.clients ) of
    % If not, just continue
    undefined ->
      { next_state, StateName, StateData };
    % If the decease client was found, delete him from our list
    Ref ->
      DeadClient            = { Pid, Ref },
      remove_client( StateData#state.clients, DeadClient )
  end.

remove_client( [Owner|[Next|Rest]], DeadClient ) when Owner == DeadClient ->
  error_logger:info_msg("Replaced Owner~n"),
  gen_fsm:reply( Next, ok ),
  { next_state, busy, #state{ clients = [Next|Rest] } };

remove_client( [Owner|Rest], DeadClient ) when Owner =/= DeadClient ->
  error_logger:info_msg("Removed Client~n"),
  { next_state, busy, #state{ clients = lists:delete( DeadClient, Rest ) } };

remove_client( [_Owner|[]], _DeadClient ) ->
  error_logger:info_msg("Removed Owner and freed mutex~n"),
  { next_state, free, #state{ clients = [] } }.

code_change( _Foo, _Bar, _Baz, _Bang ) ->
  ok.

terminate( _Foo, _Bar, _Baz ) ->
  ok.

start() ->
  gen_fsm:start( {local, mutex}, ?MODULE, [], [] ).

wait() ->
  ok = gen_fsm:sync_send_event( mutex, give_me_resource, infinity ).

signal() ->
  ok = gen_fsm:sync_send_event( mutex, release, infinity ),
  ok.

% should be synchronous
free( _Event, From, StateData ) ->
  error_logger:info_msg("In Free State ~n"),
  error_logger:info_msg("State: ~p ~n", [StateData] ),
  { Pid, _ } = From,
  link(Pid),
  { reply, ok, busy, StateData#state{ clients = [From] } }.

% should be synchronous
busy( Event, From, StateData ) ->
  error_logger:info_msg("In Busy State ~n"),

  case Event of
    give_me_resource ->
      WaitingList = lists:append( StateData#state.clients, [From] ),
      error_logger:info_msg("Resource busy added to waiting list: ~p ~n", [WaitingList]),

      { next_state, busy, StateData#state{ clients = WaitingList } };
    release -> release_mutex( StateData#state.clients )
  end.

release_mutex( [_Owner|[Next|Rest]] ) ->
  gen_fsm:reply(Next, ok),
  { reply, ok, busy, #state{ clients = [Next|Rest] } };

release_mutex( [Owner | []] ) ->
  error_logger:info_msg("Free Willy!!!~n"),
  error_logger:info_msg("WOOOOOT: ~p ~n", [Owner]),
  { reply, ok, free, #state{ clients = [] } }.



