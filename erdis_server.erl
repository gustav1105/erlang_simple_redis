-module(erdis_server).
-export([start/0, stop/0]).

-record(entry, {key, value}).

init_db() ->
	mnesia:create_schema([node()]),
	mnesia:start(),
	mnesia:create_table(entry, [{attributes, record_info(fields, entry)}]).

start() ->
	init_db(),
	{ok, Listen} = gen_tcp:listen(10101, [binary, {packet, 4}, {reuseaddr, true}, {active, true}]),
	spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
	{ok, Socket} = gen_tcp:accept(Listen),
	spawn(fun() -> par_connect(Listen) end),
	loop(Socket).

stop() ->
	mnesia:stop().

loop(Socket) ->
	receive
		{tcp, Socket, Bin} ->
			io:format("Binary received ~p~n", [Bin]),
			Str = binary_to_term(Bin),
			Reply = process_command(Str),
			BinReply = term_to_binary(Reply),
			io:format("Response  ~p~n", [BinReply]),
			gen_tcp:send(Socket, BinReply),
			loop(Socket);
		{tcp_closed, Socket} ->
			io:format("Connection Closed ~n")
	end.

process_command(Str) ->
	Parts = string:split(Str, " ", all),
	Command = lists:nth(1, Parts),

	case Command of
		"get" -> get_value(lists:nth(2, Parts));
		"set" -> set_value(lists:nth(2, Parts), lists:nth(3, Parts));
		"size" -> get_size();
		"keys" -> keys();
		"delete" -> delete(lists:nth(2, Parts));
		"exists" -> exists(lists:nth(2, Parts))
	end.

get_value(Key) ->
	F = fun() ->
				Oid = {entry, Key},
				Entries = mnesia:read(Oid),
				case Entries =:= [] of
					true -> does_not_exists;
					false -> lists:nth(1, Entries)
				end
			end,
		mnesia:transaction(F).

set_value(Key, Value) ->
	Entry = #entry{key = Key, value = Value},
	F = fun() -> 
				mnesia:write(Entry)
		end,
	mnesia:transaction(F),
	{ok, Key, Value}.

get_size() ->
	F = fun() ->
				mnesia:table_info(entry, size)
		end,
	{ok, mnesia:transaction(F)}.

keys() ->
	F = fun() ->
				mnesia:all_keys(entry)
		end,
	{ok, mnesia:transaction(F)}.

delete(Key) ->
	Oid = {entry, Key},
	F = fun() ->
				mnesia:delete(Oid)
		end,
	mnesia:transaction(F),
	{ok, Key}.

exists(Key) ->
	Exists = get_value(Key),
	case Exists of
		{atomic, does_not_exists} -> false;
		{atomic, _} -> true
	end.
