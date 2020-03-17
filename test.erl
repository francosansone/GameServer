-module(test).
-compile(export_all).

test_listener(Value, Port) ->
    global:register_name("P" ++ Value, self()),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}, {packet,0}]),
    dispatcher(Value, ListenSocket).


dispatcher(Value, ListenSocket) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    receive
        {tcp, Socket, Msg} ->
            io:format("~p receiving ~p~n", [self(), Msg]),
            gen_tcp:send(Socket, Value),
            dispatcher(Value, ListenSocket)
    end.

-record(url, {ip='localhost', port=9000}).

server_list() ->
    [#url{}, #url{port=9001}].

printer(L) ->
    case L of
        [Val | Rest] ->
            io:format("~s ~p~n", [Val#url.ip, Val#url.port]),
            printer(Rest);
        [] ->
            io:format("end~n")
    end.

test_sender() ->
    L = server_list(),
    io:format("list is ~p~n", [L]),
    test_sender_names(L, []).

test_sender_names(L, Values) ->
    case L of
        [Name | LL] ->
            Ip = Name#url.ip,
            Port = Name#url.port,
            io:format("connecting to ~s ~p~n", [Ip, Port]),
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active,true}, {packet,0}]),
            io:format("connected to ~s ~p~n", [Ip, Port]),
            gen_tcp:send(Socket, "ok"),
            receive
                {tcp, Socket, Value} ->
                    io:format("received ~p~n", [Value]),
                    test_sender_names(LL, Values ++ [Value]);
                {error, Reason} -> io:format("error ~s~n",[Reason])
            end;
        [] ->
            io:format("list ended ~p~n", [Values])
    end.

