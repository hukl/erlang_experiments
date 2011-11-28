-module( mutex_client ).

-behaviour( gen_server ).

% Public API
-export([ start/1, grab/0, release/0 ]).

% gen_server callbacks
-export([ init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
          code_change/3 ]).


start( Name ) ->
  gen_server:start_link( {local, Name}, ?MODULE, [], []).

grab() ->
  mutex:wait().

release() ->
  mutex:signal().


init( [] ) ->
  { ok, [] }.

handle_call( _Request, _From, _State ) ->
  { reply, ok, [] }.

handle_cast( Request, _State ) ->
  case Request of
    grab ->
      error_logger:info_msg("Oh hi from ~p with request ~p ~n", [self(), Request]),
      ok = mutex:wait(),
      error_logger:info_msg("Grabbed the Mutex!!!~n");
    release ->
      ok = mutex:signal(),
      error_logger:info_msg("Released the Mutex!!!~n");
    stop ->
      exit(normal)
    end,
  { noreply, ok }.

handle_info( _Info, _State ) ->
  { noreply, [] }.

terminate( _Reason, _State ) ->
  whatever.

code_change( _OldVsn, _State, _Extra ) ->
  { ok, [] }.