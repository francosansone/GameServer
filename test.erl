-module(test).
-compile(export_all).

test_listener(Value, Port) ->
    global:register_name("P" ++ Value, self()),
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,true}, {packet,0}]),
    receive
        {ok, Pid} ->
            io:format("~p receiving ~p~n", [self(), Value]),
            Pid!Value
    end,
    test_listener(Value).


test_sender() ->
    L = global:registered_names(),
    io:format("list is ~p~n", [L]),
    test_sender_names(L, []).

test_sender_names(L, Values) ->
    case L of
        [Name | LL] ->
            Pid = global:whereis_name(Name),
            Pid!{ok, self()},
            receive
                Value -> test_sender_names(LL, Values ++ [Value])
            end;
        [] ->
            io:format("list ended ~p~n", [Values])
    end.
