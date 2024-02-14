-module(clientTest).
-import(client, [start/1, close/1, put/3,
  get/2]).
-export([get_val/0, start_putkey/0, start_putval/0, get_elem/2]).

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

get_elem(Key, X) ->
    Pid = client:start(localhost),
    Val = client:get(Pid, Key),
    %file:write_file("recibidos/" ++ 
    %    integer_to_list(X) ++ " sep " ++
    %    pid_to_list(Pid) ++ ".txt",
    %    io_lib:fwrite("~p. \n", [Val])),
    io:fwrite("~p) Recibi ~p bytes ~n",
        [X, byte_size(term_to_binary(Val))]),
    %string:length(Val),
    client:close(Pid).

spawn_processes(0) ->
    spawn(clientTest, get_elem, [str, 0]);
spawn_processes(X) ->
    spawn(clientTest, get_elem, [str, X]),
	%sleep(500),
    spawn_processes(X-1).

sleep(Duration) ->
    receive
        wake_up -> ok
    after Duration ->
        ok
    end.

start_putval() ->
    Pid = client:start(localhost),
    Str = readlines("bible_copy.txt"),
    StrBin = term_to_binary(Str),
    io:format("~p~n", [byte_size(StrBin)]),
    put(Pid, str, Str),
    %sleep(5000),
    client:close(Pid).
		%spawn_processes(100).

start_putkey() ->
		Pid = client:start(localhost),
    Str = readlines("bible_copy.txt"),
    StrBin = term_to_binary(Str),
    io:format("~p~n", [byte_size(StrBin)]),
    put(Pid, Str, str),
    %sleep(5000),
    client:close(Pid).

get_val() ->
		Pid = client:start(localhost),
    Str = readlines("bible_copy.txt"),
    StrBin = term_to_binary(Str),
    io:format("~p~n", [byte_size(StrBin)]),
    ToPrint = get(Pid, Str),
    %sleep(5000),
    client:close(Pid),
		ToPrint.

