-module(client).

-export([start/1, close/1, put/3, get/2]).

% start: inet:socket_address() | inet:hostname() -> socket()
% Se conecta a un servior en el puerto TCP 889,
% en el host con dirección IP Address, Devuelve
% el socket de la conexión.
start(Address) ->
    {ok, Sock} = gen_tcp:connect(Address, 889,
                    [binary, {packet, 0}]),
                        %,{active,false}]), %me da error
    Sock.

% close: socket() -> ok
% Dado un socket cierra la conección.
close(Sock) ->
    gen_tcp:close(Sock).

% list_length: List -> Int
% Dada una lista devuelve la cantidad
% de elementos de la misma.
list_length([]) ->
    0;
list_length([_First|Rest]) ->
    1 + list_length(Rest).

% get_put_response: socket() -> int
% Obtiene la respuesta al pedido put
get_put_response(Sock) ->  
    receive
    {tcp, Sock, Data} ->
        binary_to_term(Data)
    end.  

% put: socket(), Any, Any -> ok | error
% Manda un paquete por Sock en modo binario,
% en el cual en el primer byte está el código,
% los siguientes 4 la longitud de la clave,
% luego la clave, luego de la clave hay 
% 4 bytes que indican la longitud del valor
% y por último está el valor
put(Sock, K, V) ->
    Code = 11,
    KeyBin = term_to_binary(K), 
    ValBin = term_to_binary(V),
    LengthK = byte_size(KeyBin),
    LengthV = byte_size(ValBin),
    Packet = <<Code:8/big-unsigned-integer,
             LengthK:32/big-unsigned-integer,
             KeyBin/binary,
             LengthV:32/big-unsigned-integer,
             ValBin/binary>>,
    gen_tcp:send(Sock, Packet),
    Response = get_put_response(Sock),
    case Response of
        101 -> ok;
        _ -> error
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

% readData: List, Int, Int -> List
% Lee del socket y lo agrega a Value hasta
% que su longitud sea igual a LenToAchive
% devuelve la lista resultante.
readData(Value, LenValue, LenToAchive) ->
    receive
    {tcp,_Sock,Data} ->
        ListData = binary_to_list(Data),
        LenListData = list_length(ListData),
        Val = lists:append(Value, ListData),
        ValLen = LenListData + LenValue,
        if 
            ValLen == LenToAchive ->
                Val;
            ValLen >= LenToAchive ->
                lists:sublist(Val, 1, LenToAchive);
            ValLen < LenToAchive ->
                readData(Val, ValLen, LenToAchive)
        end
    end.

% get_get_response: Sock -> {term, Any} | term
% Dado un socket obtiene la respuesta del get del
% servidor, devuelve {ok, Any} si respondió con
% exito a la petición, einval si no encontró el
% valor asociado a la clave y error en otro caso    
get_get_response(Sock) ->
    receive
    {tcp,Sock,Data} ->
        ListData = binary_to_list(Data),
        Code = lists:sublist(ListData,1,1),
        io:format("getResponse: code=~p~n" ,[Code]),
        if
            Code == [101] ->
                LenVal = lists:sublist(Data,2,4),
                Len = trunc(lenToInt(LenVal,3)),
                {_InfPack, Value} = lists:split(5,ListData),
                CurrLenValue = list_length(Value),
                if
                    Len == CurrLenValue ->
                        {ok, binary_to_term(list_to_binary(Value))};
                    Len <= CurrLenValue ->
                        binary_to_term(list_to_binary(
                            lists:sublist(Value, 1, LenToAchive)));
                    CurrLenValue < Len -> 
                        {ok, 
                        binary_to_term(list_to_binary(
                        readData(Value,
                        CurrLenValue,
                        Len)))}
                end;
            Code == [112] ->
                enotfound;
            true ->
                error
        end
    end.

% get: sock(), Any -> {ok, Any} | atom
% Dado un socket y una clave, si encontró
% el valor asociado a la clave devuelve
% el valor asociado, si no lo encontró
% devuelve enotfound y error en otro caso.
get(Sock, K) ->
    Code = 13,
    KeyBin = term_to_binary(K),
    LengthK = byte_size(KeyBin),
    Packet = <<Code:8/unsigned-integer, 
               LengthK:32/big-unsigned-integer,
               KeyBin/binary>>,
    gen_tcp:send(Sock, Packet),
    get_get_response(Sock).
