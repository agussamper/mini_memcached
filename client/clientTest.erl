-module(clientTest).
-import(client, [start/1, close/1, put/3,
  get/2]).
-export([start/0, get_elem/1]).

readlines(FileName) ->
    {ok, Device} = file:open(FileName, [read]),
    try get_all_lines(Device)
      after file:close(Device)
    end.

get_all_lines(Device) ->
    case io:get_line(Device, "") of
        eof  -> [];
        Line -> Line ++ get_all_lines(Device)
    end.

get_elem(Key) ->
    Pid = client:start(localhost),
    Val = client:get(Pid, Key),
    file:write_file(pid_to_list(Pid) ++ ".txt",
        io_lib:fwrite("~p. \n", [Val])),
    client:close(Pid).

spawn_processes(0) ->
    spawn(clientTest, get_elem, [str]);
spawn_processes(X) ->
    spawn(clientTest, get_elem, [str]),
    spawn_processes(X-1).

sleep(Duration) ->
    receive
        wake_up -> ok
    after Duration ->
        ok
    end.

start() ->
    Pid = client:start(localhost),
    Str = readlines("bible_copy.txt"),
    StrBin = term_to_binary(Str),
    io:format("~p~n", [byte_size(StrBin)]),
    put(Pid, str, Str),
    sleep(10000),
    client:close(Pid).
    %spawn_processes(10).
