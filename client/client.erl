-module(client).

-export([start/1, close/1, put/3,
    get/2, del/2, stats/1]).

-define(PUT,11).
-define(DEL,12).
-define(GET,13).
-define(STATS,21).
-define(OK,101).
-define(EINVAL,111).
-define(ENOTFOUND,112).
-define(EBINARY,113).
-define(EBIG,114).
-define(EUNK, 115).

% start: inet:socket_address() | inet:hostname() -> socket()
% Se conecta a un servior en el puerto TCP 889,
% en el host con dirección IP Address, Devuelve
% el proceso asociado a la conexión.
start(Address) ->
    case gen_tcp:connect(Address, 8889,
        [binary, {packet,0}, {active, false}])
            of
        {ok, Sock} -> spawn(fun()->requestListener(Sock) end);
        {error, Reason} -> {error, Reason}
    end.

% lenToInt: List, Int -> Int
% Dado una lista, y la longitud de la lista
% menos uno, hace lo siguiente
% List[X]*256**X+List[X-1]*256**X-1+...
% así sucesivamente hasta que X=0.
% Devuelve el resultado de la operación
lenToInt([Head|Rest], X) ->
    lenToInt(Rest,X-1)+Head*math:pow(256,X);
lenToInt([], -1) ->
    0.

% getLen: sock() -> Int
% Dado un socket, recibe 4 bytes del mismo
% y aplica la función lenToInt
getLen(Sock) ->
    {ok, BinLen} = gen_tcp:recv(Sock,4),
    io:format("BinLenList=~p~n", [binary_to_list(BinLen)]),
    trunc(lenToInt(
        binary_to_list(
            BinLen),
            3)).

% getNum: sock(), Int -> Int
% Dado un socket, lee Bytes bytes
% del mismo, y aplica la función
% lenToInt para obtener el entero
getNum(Sock, Bytes)  ->
    trunc(lenToInt(
        binary_to_list(
            gen_tcp:recv(Sock,Bytes)),
            Bytes-1)).

% response: sock(), atom, pid(), binary() -> atom | {atom, term}
% Envia Packet al servidor a través de Sock,
%  recibe la respuesta del mismo y la devuelve
response(Sock, {Ins, _Id, Packet}) ->
    gen_tcp:send(Sock, Packet),
    {ok,Code} = gen_tcp:recv(Sock,1),
    case Ins of
        put -> 
            case Code of
                <<?OK>> -> ok;
                _ -> {error, Code}
            end;            
        get ->
            case Code of
                <<?OK>> ->
                    Len = getLen(Sock),
                    io:format("~p~n",[Len]),
                    {ok, BinVal} = gen_tcp:recv(Sock, Len),
                    io:format("ya lei~n"),
                    Val = binary_to_term(BinVal),
                    {ok,Val};
                <<?ENOTFOUND>> ->
                    enotfound;
                _ -> {error, Code}
            end;
        del ->
            case Code of
                <<?OK>> -> ok;
                <<?ENOTFOUND>> -> enotfound;
                _ -> {error, Code}
            end;
        stats ->
            case Code of
                <<?OK>> ->
                    Puts = integer_to_list(getNum(Sock, getLen(Sock))),
                    Dels = integer_to_list(getNum(Sock, getLen(Sock))),
                    Gets = integer_to_list(getNum(Sock, getLen(Sock))),
                    Keys = integer_to_list(getNum(Sock, getLen(Sock))),
                    "OK PUTS=" ++ Puts ++ " DELS=" ++ Dels ++ 
                        " GETS=" ++ Gets ++ " KEYS=" ++ Keys;
                _ -> {error, Code}
            end            
    end.

% requestListener: Sock -> ok
% Queda en espera hasta que recibe
% instrucciones para mandar al servidor
% via Sock, luego vuelve a su estado
% inicial. En el caso que la instrucción
% sea close sale de la función devolviendo
% ok 
requestListener(Sock) ->
    receive
        {Ins, Id, Packet} ->
            Id!response(Sock, {Ins, Id, Packet}),
            requestListener(Sock);
        close ->
            gen_tcp:close(Sock)
    end.

% close: socket() -> ok
% Cierra la conexión asociada con el id
% del proceso dado.
close(Pid) ->
    Pid!close.

recv_resp() ->
    receive
        Data->Data
    end.

% put: pid(), Any, Any -> ok | error
% Agrega la clave valor en modo binario a 
% la instancia de cache asociado a pid,
% en el cual en el primer byte está el código,
% los siguientes 4 la longitud de la clave,
% luego la clave, luego de la clave hay 
% 4 bytes que indican la longitud del valor
% y por último está el valor
put(Pid, K, V) ->
    Code = ?PUT,
    KeyBin = term_to_binary(K), 
    ValBin = term_to_binary(V),
    LengthK = byte_size(KeyBin),
    LengthV = byte_size(ValBin),
    io:format("LengthK=~p~nKeyBin=~p~nLengthV=~p~nValBin=~p~n",
        [LengthK, KeyBin, 
            LengthV, ValBin]),
    Packet = <<Code:8/unsigned-integer,
             LengthK:32/big-unsigned-integer,
             KeyBin/binary,
             LengthV:32/big-unsigned-integer,
             ValBin/binary>>,
    case is_process_alive(Pid) of
        true ->
            Pid!{put,self(),Packet},
            recv_resp()
    end.

% get: pid(), Any -> {ok, Any} | atom
% Dado un id de un proceso y una clave,
% si encontró el valor asociado a la clave
% de la instancia de cache asociada a pid
% devuelve el valor asociado, si no lo encontró
% devuelve enotfound.
get(Pid, K) ->
    Code = ?GET,
    KeyBin = term_to_binary(K),
    LengthK = byte_size(KeyBin),
    Packet = <<Code:8/unsigned-integer, 
               LengthK:32/big-unsigned-integer,
               KeyBin/binary>>,
    case is_process_alive(Pid) of 
        true ->
            Pid!{get,self(),Packet},
            recv_resp()
    end.

% del: pid(), Ant -> {ok, Any} | atom
% Dado un id de un proceso y una clave,
% si encontró la clave de la instancia 
% de cache asociada a pid elimina la
% clave y su valor asociado de la misma
% y devuelve ok, si no lo encuentra
% devuelve enotfound
del(Pid, K) ->
    Code = ?DEL,
    KeyBin = term_to_binary(K),
    LengthK = byte_size(KeyBin),
    Packet = <<Code:8/unsigned-integer,
               LengthK:32/big-unsigned-integer,
               KeyBin/binary>>,
    case is_process_alive(Pid) of
        true ->
            Pid!{del,self(),Packet},
            recv_resp()
    end.

% del: pid(), Ant -> {ok, Any} | atom
% Dado un id de un proceso, devuelve
% las estadísticas de la instancia 
% de cache asociada al pid.
stats(Pid) ->
    Code = ?STATS,
    Packet = <<Code:8/unsigned-integer>>,
    case is_process_alive(Pid) of 
        true ->
            Pid!{stats,self(),Packet},
            recv_resp()
    end.
