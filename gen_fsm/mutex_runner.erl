-module(mutex_runner).

-compile( export_all ).

test_1() ->
  { ok, _Mutex }   = mutex:start(),
  { ok, _Client1 } = mutex_client:start(client_1),
  { ok, _Client2 } = mutex_client:start(client_2),
  grab( client_1 ),
  grab( client_2 ),

  release( client_1 ),
  release( client_2 ).

test_2() ->
  { ok, _Mutex }   = mutex:start(),
  { ok, _Client1 } = mutex_client:start(client_1),
  { ok, _Client2 } = mutex_client:start(client_2),
  grab( client_1 ),
  grab( client_2 ),

  gen_server:cast( client_1, stop ).

test_3() ->
  { ok, _Mutex }   = mutex:start(),
  { ok, _Client1 } = mutex_client:start(client_1),
  { ok, _Client2 } = mutex_client:start(client_2),

  grab( client_1 ),
  grab( client_2 ),

  gen_server:cast( client_2, stop ).



grab( Name ) ->
  error_logger:info_msg("Grabbing the mutex with ~p ~n", [Name]),
  gen_server:cast( Name, grab ).

release( Name ) ->
  error_logger:info_msg("Releasing the mutex with ~p ~n", [Name]),
  gen_server:cast( Name, release ).