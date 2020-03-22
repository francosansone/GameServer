-module(server).
-compile(export_all).


-record(gameState, {
    id="",
    player1="",
    player2="",
    watchers=[],
    state={}
}).

-record(url, {id="server0", ip='localhost', port=8000}).

server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,false}, {packet,0}]),
    io:format("Listening in port ~p~n", [Port]),
    initControllers(Port),
    dispatcher(ListenSocket, Port).

initControllers(Port) ->
    UserNamesPid = spawn(?MODULE, userNames, [[]]),
    CoopUserNameReq = spawn(?MODULE, cooperativeUserNameReq, ['localhost', Port]),
    GameListPid = spawn(?MODULE, gameList, [[]]),
    CoopGameListPid = spawn(?MODULE, cooperativeGameListReq, [Port]),
    GameCounterPid = spawn(?MODULE, gameCounter, [Port]),
    global:register_name("name_list", UserNamesPid),
    global:register_name("cooperative_user_name", CoopUserNameReq),
    global:register_name("game_list", GameListPid),
    global:register_name("cooperative_game_list", CoopGameListPid),
    global:register_name("game_counter", GameCounterPid).

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
                true -> gen_tcp:send(Socket, "EXISTS COOP NAME");
                false -> gen_tcp:send(Socket, "DOESNT EXISTS COOP NAME")
            end;
        "NEW " ++ Name ->
            io:format("~s: ~s~n", [Name, Command]),
            %% First, check if name exists
            case checkIfNameExists(Name) of
                true ->
                    GameListPid = global:whereis_name("game_list"),
                    GameListPid!{add, Name, self()},
                    receive
                        {ok, GameId} -> gen_tcp:send(Socket, "OK NEW " ++ GameId);
                        _ -> gen_tcp:send(Socket, "ERROR NEW")
                    end;
                _ -> gen_tcp:send(Socket, "ERROR NEW")
            end;
        "LSG " ++ Name ->
            io:format("LSG ~s ~n", [Name]),
            GameListPid = global:whereis_name("game_list"),
            GameListPid!{get, self()},
            receive
                L ->
                    io:format("llego nomas ~p~n", [L]),
                    gen_tcp:send(Socket, "OK LSG " ++ L)
            end;
        "COOP LSG" ->
            io:format("COOP LSG~n"),
            GameListPid = global:whereis_name("game_list"),
            GameListPid!{getCoop, self()},
            receive
                L ->
                    io:format("llego nomas ~p~n", [L]),
                    gen_tcp:send(Socket, "OK COOP LSG " ++ L)
            end;
        "ACC " ++ (Rest) ->
            io:format("ACC ~s~n", [Rest]),
            Rest_ = string:split(Rest, " "),
            case length(Rest_) of
                2 ->
                    Name = lists:nth(1, Rest_),
                    GameId = lists:nth(2, Rest_),
                    case checkIfNameExists(Name) of
                        false -> gen_tcp:send(Socket, "ERROR ACC");
                        true ->
                            GameListPid = global:whereis_name("game_list"),
                            GameListPid!{addPlayer, Name, GameId, self()},
                            receive
                                ok -> gen_tcp:send(Socket, "OK ACC");
                                error -> gen_tcp:send(Socket, "ERROR ACC")
                            end
                    end;
                _ ->
                    io:format("Bad command~n"),
                    gen_tcp:send(Socket, "ERROR ACC")
            end;
        _ ->
            io:format("Unexpected~n"),
            gen_tcp:send(Socket, "ERROR No Implementado")
    end.


%% Process with lists of server

totalServerList() ->
    [
        #url{},
        #url{id = "server1", port=8001}
%%        #url{id = 'server2', port=8002}
    ].

serverList(Ip, Port) ->
    L = totalServerList(),
    LL = lists:filter(fun(X)->
        P = X#url.port /= Port,
        I = X#url.ip /= Ip,
        P or I end, L),
    LL.


%% Process to keep list of names

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
                    COOP!{contains, Name, self()},
                    receive
                        false ->
                            Pid!ok,
                            userNames(UsernameList ++ [Name]);
                        true ->
                            Pid!error,
                            userNames(UsernameList);
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
        {contains, Name, Pid} ->
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
                {ok, "DOESNT EXISTS COOP NAME"} ->
                    gen_tcp:close(Socket),
                    userNameReq(Rest, Name);
                {ok, "EXISTS COOP NAME"} ->
                    gen_tcp:close(Socket),
                    true;
                _ ->
                    gen_tcp:close(Socket),
                    error
            end;
        [] -> false
    end.

attendToCoopNameReq(Name) ->
    UserNamesPid = global:whereis_name("name_list"),
    UserNamesPid!{contains, Name, self()},
    io:format("attendToCoopNameReq~n"),
    receive
        Val -> Val
    end.


checkIfNameExists(Name) ->
    UserNamesPid = global:whereis_name("name_list"),
    UserNamesPid!{contains, Name, self()},
    io:format("checkIfNameExists~n"),
    receive
        true -> true;
        false ->
            COOP = global:whereis_name("cooperative_user_name"),
            COOP!{contains, Name, self()},
            receive
                Val -> Val
            end
    end.

% Process with list of games

%% keep list of games in this server

gameCounter(Port) ->
    gameCounter('localhost', Port).


gameCounter(Ip, Port) ->
    Servers = totalServerList(),
    LL = lists:filter(fun(X)->
        P = X#url.port == Port,
        I = X#url.ip == Ip,
        io:format("filtering ~s ~p with ~s ~p~n", [I, P, Ip, Port]),
        P and I end, Servers),
    S = lists:nth(1, LL),
    Prefix = S#url.id,
    io:format("~s~n", [Prefix]),
    gameCounterPrefix(0, Prefix).


gameCounterPrefix(N, Prefix) ->
    receive
        {add, Name, Pid, PCommandPid} ->
            StrN = lists:flatten(io_lib:format("~p", [N])),
            Pid!{new, Name, Prefix ++ ("_" ++ StrN), PCommandPid},
            gameCounterPrefix(N + 1, Prefix)
    end.

gameList(GameList) ->
    receive
        {add, Name, Pid} ->
            io:format("gameList receive add~n"),
            GameCounterPid = global:whereis_name("game_counter"),
            GameCounterPid!{add, Name, self(), Pid},
            gameList(GameList);
        {new, Name, GameId, Pid} ->
            Pid!{ok, GameId},
            Game = #gameState{
                id = GameId,
                player1 = Name
            },
            io:format("adding new game ~s ~s~n", [Game#gameState.id, Game#gameState.player1]),
            gameList(GameList ++ [Game]);
        {get, Pid} ->
            CoopGameListPid = global:whereis_name("cooperative_game_list"),
            CoopGameListPid!{get, Pid, self()};
        {getCoop, Pid} ->
            L = parseGameList(GameList),
            Pid!L,
            gameList(GameList);
        {gameList, PCommandPid, Games} ->
            TotalListGames = (parseGameList(GameList)) ++ Games,
            PCommandPid!TotalListGames;
        {addPlayer, Name, GameId, Pid} ->
            case isAddPlayerAvailable(GameList, Name, GameId) of
                {ok, NewGameList} ->
                    Pid!ok,
                    gameList(NewGameList);
                {error, Reason} ->
                    io:format("Error adding player ~s~n", [Reason]),
                    Pid!error,
                    gameList(GameList)
            end

    end,
    gameList(GameList).

parseGameList(L) ->
    parseGameList(L, "").


parseGameList(GameList, Accumulator) ->
    case GameList of
        [Game | Rest] ->
            Id = "Id: " ++ Game#gameState.id,
            P1 = " Player 1: " ++ Game#gameState.player1,
            P2 = " Player 2: " ++ (case Game#gameState.player2 of
                                      "" -> "---";
                                      Str -> Str
                                  end),
            StrGame = ("\n" ++ (Id ++ (P1 ++ (P2)))),
            parseGameList(Rest, StrGame ++ (Accumulator));
        [] -> Accumulator
    end.


cooperativeGameListReq(Port) ->
    cooperativeGameListReq('localhost', Port).


cooperativeGameListReq(Ip, Port) ->
    receive
        {get, PCommandPid, Pid} ->
            Servers = serverList(Ip, Port),
            spawn(?MODULE, gameListReq, [Servers, PCommandPid, Pid])
    end,
    cooperativeGameListReq(Ip, Port).


gameListReq(Servers, PCommandPid, Pid) ->
    gameListReq(Servers, PCommandPid, Pid, []).


gameListReq(Servers, PCommandPid, Pid, Games) ->
    case Servers of
        [Server | Rest] ->
            Ip = Server#url.ip,
            Port = Server#url.port,
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active,false}, {packet,0}]),
            gen_tcp:send(Socket, "COOP LSG"),
            case gen_tcp:recv(Socket, 0) of
                {ok, "OK COOP LSG " ++ L} ->
                    gen_tcp:close(Socket),
                    gameListReq(Rest, PCommandPid, Pid, Games ++ L);
                _ ->
                    gen_tcp:close(Socket),
                    error
            end;
        [] -> Pid!{gameList, PCommandPid, Games}
    end.

isAddPlayerAvailable(GameList, Name, GameId) ->
    isAddPlayerAvailable([], GameList, Name, GameId).

isAddPlayerAvailable(Front, Back, Name, GameId) ->
    case Back of
        [] -> {error, "Game does not exists"};
        [Game | Rest] ->
            io:format("comparing ~s ~s~n", [Game#gameState.id, GameId]),
            if
                Game#gameState.id == GameId ->
                    if
                        Game#gameState.player2 == "" ->
                            UpdGame = #gameState{
                                id = Game#gameState.id,
                                player1 = Game#gameState.player1,
                                player2 = Name,
                                watchers = Game#gameState.watchers,
                                state = Game#gameState.state
                            },
                            io:format("new record with ~s ~s ~s~n", [UpdGame#gameState.id, UpdGame#gameState.player1, UpdGame#gameState.player2]),
                            L = Front  ++ [UpdGame | Rest],
                            io:format("printing list ~p~n", [L]),
                            {ok, L};

                        true ->
                            {error, "Game was occupied"}
                    end;
                true -> isAddPlayerAvailable([Front | [Game]], Rest, Name, GameId)
            end
    end.