-module(server).
-compile(export_all).

server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,false}, {packet,0}]),
    io:format("Listening in port ~p~n", [Port]),
    dispatcher(ListenSocket, Port).

dispatcher(ListenSocket, Port) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, psocket, [Socket]),
    dispatcher(ListenSocket, Port).

psocket(Socket) ->
    io:format("psocket, ~p~n", [Socket]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            spawn(?MODULE, pcommand, [self(), Command]),
            receive
                {ok, Result} ->
                    gen_tcp:send(Socket, Result);
                _ ->
                    io:format("Error in psocket")
            end,
            psocket(Socket);
        {error, Reason} ->
            io:format("closed ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end.

pcommand(Parent, Command) ->
    io:format("pcommand ~p~n", [Command]),
    case Command of
        "-CON " ++ Name ->
            io:format("CON ~s~n", [Name]),
            Parent!{ok, "OK CON " ++ Name};
        _ ->
            io:format("Unexpected~n"),
            Parent!{ok, "ERROR No implementado"}
    end.
