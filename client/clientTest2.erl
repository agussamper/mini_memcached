-module(clientTest2).
-import(client2, [start/1, close/1, put/3]).
-export([put_values/0]).

sleep(Duration) ->
    receive
        wake_up -> ok
    after Duration ->
        ok
    end.

aux_putValues(Pid,0) ->
  client2:put(Pid, 0, 
    rand:uniform());
aux_putValues(Pid, X) ->
  client2:put(Pid, X, rand:uniform()),
  aux_putValues(Pid, X-1).

put_values() ->
  Pid = client2:start(localhost),
  aux_putValues(Pid, 100000),
  client2:close(Pid).