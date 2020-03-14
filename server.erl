-module(server).
-compile(export_all).

server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,false}, {packet,0}]),
    io:format("Listening in port ~p~n", [Port]),
    UserNamesPid = spawn(?MODULE, usernames, [[]]),
    global:register_name("name_list", UserNamesPid),
    dispatcher(ListenSocket, Port).


dispatcher(ListenSocket, Port) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, psocket, [Socket]),
    dispatcher(ListenSocket, Port).


psocket(Socket) ->
    io:format("psocket, ~p~n", [Socket]),
    case gen_tcp:recv(Socket, 0) of
        {ok, "CON " ++ Name} ->
            io:format("Recibe nombre ~s~n", [Name]),
            UserNamesPid = global:whereis_name("name_list"),
            UserNamesPid!{newName, Name, self()},
            receive
                ok ->
                    io:format("ok~n"),
                    gen_tcp:send(Socket, "OK " ++ ("CON " ++ Name)),
                    psocket(Socket, Name);
                error ->
                    io:format("error~n"),
                    gen_tcp:send(Socket, "ERROR " ++ ("CON " ++ Name)),
                    psocket(Socket)
            end;
        {error, Reason} ->
            io:format("closed ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end.


usernames(UsernameList) ->
    receive
        {newName, Name, Pid} ->
            % agregar aca un chequeo a los demas servidores que esten corriendo
            case lists:member(Name, UsernameList) of
                 true ->
                    Pid!error,
                    usernames(UsernameList);
                false ->
                    Pid!ok,
                    io:format("new name!~s~n", [Name]),
                    usernames(UsernameList ++ [Name])
            end
    end.



psocket(Socket, UserName) ->
    io:format("psocket, ~p~n", [Socket]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            spawn(?MODULE, pcommand, [self(), Command, UserName]),
            receive
                {ok, Result} ->
                    gen_tcp:send(Socket, Result);
                _ ->
                    io:format("Error in pcommand")
            end,
            psocket(Socket, UserName);
        {error, Reason} ->
            io:format("closed ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end.

pcommand(Parent, Command, UserName) ->
    io:format("pcommand ~p~n", [Command]),
    case Command of
        "LSG " ++ Name ->
            io:format("LSG ~s ~s~n", [Name, UserName]),
            Parent!{ok, "OK LSG " ++ Name};
        "NEW " ++ Name ->
            io:format("~s: ~s~n", [Name, Command]),
            Parent!{ok, "OK " ++ Command};
        "ACC " ++ Rest ->
            io:format("~s: ~s~n", [UserName, Command]),
            Parent!{ok, "OK " ++ Command};
        _ ->
            io:format("Unexpected~n"),
            Parent!{ok, "ERROR No implementado"}
    end.
