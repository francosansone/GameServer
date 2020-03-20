-module(server).
-compile(export_all).


-record(gameState, {
    player1="",
    player2="",
    watchers=[],
    state={}
}).

-record(url, {ip='localhost', port=8000}).

server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,false}, {packet,0}]),
    io:format("Listening in port ~p~n", [Port]),
    UserNamesPid = spawn(?MODULE, userNames, [[]]),
    CoopUserNameReq = spawn(?MODULE, cooperativeUserNameReq, ['localhost', Port]),
    global:register_name("name_list", UserNamesPid),
    global:register_name("cooperative_user_name", CoopUserNameReq),
    dispatcher(ListenSocket, Port).


dispatcher(ListenSocket, Port) ->
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    spawn(?MODULE, psocket, [Socket]),
    dispatcher(ListenSocket, Port).


psocket(Socket) ->
    io:format("psocket, ~p~n", [Socket]),
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            spawn(?MODULE, pcommand, [Command, Socket]),
            psocket(Socket);
        {error, Reason} ->
            io:format("closed ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end.


pcommand(Command, Socket) ->
    io:format("pcommand ~p~n", [Command]),
    case Command of
        "CON " ++ Name ->
            io:format("Recibe nombre ~s~n", [Name]),
            UserNamesPid = global:whereis_name("name_list"),
            UserNamesPid!{newName, Name, self()},
            receive
                ok ->
                    io:format("ok~n"),
                    gen_tcp:send(Socket, "OK " ++ ("CON " ++ Name));
                error ->
                    io:format("error~n"),
                    gen_tcp:send(Socket, "ERROR " ++ ("CON " ++ Name))
            end;
        "COOP NAME " ++ Name ->
            io:format("receive cooperative req ~s~n", [Name]),
            case attendToCoopNameReq(Name) of
                ok -> gen_tcp:send(Socket, "OK COOP NAME");
                error -> gen_tcp:send(Socket, "ERROR COOP NAME")
            end;
        "LSG " ++ Name ->
            io:format("LSG ~s ~n", [Name]),
            gen_tcp:send(Socket, "OK LSG " ++ Name);
        "NEW " ++ Name ->
            io:format("~s: ~s~n", [Name, Command]),
            gen_tcp:send(Socket, "OK " ++ Command);
        "ACC " ++ Rest ->
            io:format("~s~n", [Command]),
            gen_tcp:send(Socket, "OK " ++ Command);
        _ ->
            io:format("Unexpected~n"),
            gen_tcp:send(Socket, "ERROR No Implementado")
    end.


totalServerList() ->
    [
        #url{}
        #url{port=8001},
        #url{port=8002}
    ].

serverList(Ip, Port) ->
    L = totalServerList(),
    LL = lists:filter(fun(X)->
        P = X#url.port /= Port,
        I = X#url.ip /= Ip,
        P or I end, L),
    LL.

userNames(UsernameList) ->
    receive
        {newName, Name, Pid} ->
            case lists:member(Name, UsernameList) of
                true ->
                    Pid!error,
                    userNames(UsernameList);
                false ->
                    % agregar aca un chequeo a los demas servidores que esten corriendo
                    COOP = global:whereis_name("cooperative_user_name"),
                    COOP!{newName, Name, self()},
                    receive
                        ok ->
                            Pid!ok,
                            userNames(UsernameList ++ [Name]);
                        error ->
                            Pid!error,
                            userNames(UsernameList)
                    end
            end;
            {contains, Name, Pid} ->
                io:format("here we are: ~p~n", [lists:member(Name, UsernameList)]),
                Pid!lists:member(Name, UsernameList),
                userNames(UsernameList)
    end.

cooperativeUserNameReq(Ip, Port) ->
    receive
        {newName, Name, Pid} ->
            Servers = serverList(Ip, Port),
            io:format("servers ~p~n",[Servers]),
            Pid!userNameReq(Servers, Name)
    end,
    cooperativeUserNameReq(Ip, Port).

userNameReq(Servers, Name) ->
    case Servers of
        [Server | Rest] ->
            Ip = Server#url.ip,
            Port = Server#url.port,
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active,false}, {packet,0}]),
            gen_tcp:send(Socket, "COOP NAME " ++ Name),
            case gen_tcp:recv(Socket, 0) of
                {ok, "OK COOP NAME"} ->
                    gen_tcp:close(Socket),
                    userNameReq(Rest, Name);
                _ ->
                    gen_tcp:close(Socket),
                    error
            end;
        [] -> ok
    end.

attendToCoopNameReq(Name) ->
    UserNamesPid = global:whereis_name("name_list"),
    UserNamesPid!{contains, Name, self()},
    io:format("attendToCoopNameReq~n"),
    receive
        false -> ok;
        true -> error
    end.

game(R) ->
    io:format("game ~p~n", [R]).
