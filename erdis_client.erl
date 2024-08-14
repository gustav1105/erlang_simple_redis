-module(erdis_client).
-export([get/1, set/2, size/0, keys/0, delete/1, exists/1]).

client_eval(Str) ->
	{ok, Socket} = gen_tcp:connect("localhost", 10101,[binary,{packet, 4}]),
	ok = gen_tcp:send(Socket, term_to_binary(Str)),
	receive
		{tcp, Socket, Bin} ->
			io:format("~p~n",[Bin]),
			Val = binary_to_term(Bin),
			io:format("~p~n", [Val]),
			gen_tcp:close(Socket)
	end.

set(Key, Value) ->
	Command = io_lib:format("set ~s ~s", [Key, Value]),
	client_eval(Command).

get(Key) -> 
	Command = io_lib:format("get ~s",[Key]),
	client_eval(Command).

size() ->
	Command = "size",
	client_eval(Command).

keys() ->
	Command = "keys",
	client_eval(Command).

delete(Key) ->
	Command = io_lib:format("del ~s", [Key]),
	client_eval(Command).

exists(Key) ->
	Command = io_lib:format("exists ~s", [Key]),
	client_eval(Command).
