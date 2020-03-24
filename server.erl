-module(server).
-compile(export_all).


-record(gameState, {
    id="",
    player1="-",
    player2="-",
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
    CoopResolveUpdPid = spawn(?MODULE, cooperativeResolveUpdate, ['localhost', Port]),
    global:register_name("name_list", UserNamesPid),
    global:register_name("cooperative_user_name", CoopUserNameReq),
    global:register_name("game_list", GameListPid),
    global:register_name("cooperative_game_list", CoopGameListPid),
    global:register_name("game_counter", GameCounterPid),
    global:register_name("cooperative_resolve_upd", CoopResolveUpdPid).

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
                    UserPid = spawn(?MODULE, userSender, [Socket]),
                    global:register_name(Name, UserPid),
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
                            case addPlayerAvailable(Name, GameId) of
                                {ok, Game, GameStr} ->
                                    gen_tcp:send(Socket, "OK ACC " ++ GameStr),
                                    updateInterested(Game, GameStr, Name);
                                error -> gen_tcp:send(Socket, "ERROR ACC")
                            end
                    end;
                _ ->
                    io:format("Bad command~n"),
                    gen_tcp:send(Socket, "ERROR ACC")
            end;
        "OBS " ++ (Rest) ->
            io:format("OBS ~s~n", [Rest]),
            Rest_ = string:split(Rest, " "),
            case length(Rest_) of
                2 ->
                    Name = lists:nth(1, Rest_),
                    GameId = lists:nth(2, Rest_),
                    case checkIfNameExists(Name) of
                        false -> gen_tcp:send(Socket, "ERROR OBS");
                        true ->
                            case addWatcher(Name, GameId) of
                                {ok, Game, GameStr} ->
                                    gen_tcp:send(Socket, "OK OBS " ++ GameStr),
                                    updateInterested(Game, GameStr, Name);
                                error -> gen_tcp:send(Socket, "ERROR OBS")
                    end
                end;
                _ ->
                    io:format("Bad command~n"),
                    gen_tcp:send(Socket, "ERROR ACC")
            end;
        "LEA " ++ (Rest) ->
            io:format("OBS ~s~n", [Rest]),
            Rest_ = string:split(Rest, " "),
            case length(Rest_) of
                2 ->
                    Name = lists:nth(1, Rest_),
                    GameId = lists:nth(2, Rest_),
                    case checkIfNameExists(Name) of
                        false -> gen_tcp:send(Socket, "ERROR LEA");
                        true ->
                            case leaveGame(Name, GameId) of
                                {ok, Game, GameStr} ->
                                    gen_tcp:send(Socket, "OK LEA " ++ GameStr),
                                    updateInterested(Game, GameStr, Name);
                                error -> gen_tcp:send(Socket, "ERROR LEA")
                            end
                    end;
                _ ->
                    io:format("Bad command~n"),
                    gen_tcp:send(Socket, "ERROR ACC")
            end;
        "COOP GET " ++ GameId ->
            GameListPid = global:whereis_name("game_list"),
            GameListPid!{get, GameId, self()},
            receive
                {ok, Game} ->
                    io:format("Game ~s found!~n", [GameId]),
                    GameStr = gameStateToStr(Game),
                    io:format("response ~s~n", [GameStr]),
                    gen_tcp:send(Socket, "OK COOP GET " ++ GameStr);
                {error, Reason} ->
                    io:format("Game ~s not found ~s~n", [GameId, Reason]),
                    gen_tcp:send(Socket, "ERROR COOP GET")
            end;
        "COOP UPD GAME " ++ GameStr ->
            Game = strToGameState(GameStr),
            GameListPid = global:whereis_name("game_list"),
            GameListPid!{update, Game, self()},
            receive
                ok -> gen_tcp:send(Socket, "OK COOP UPD GAME");
                error -> gen_tcp:send(Socket, "ERROR COOP UPD GAME")
            end;
        "COOP UPD NAME " ++ Rest ->
            RestList = string:split(Rest, " "),
            if
                length(RestList) > 1 ->
                    Name = lists:nth(1, RestList),
                    GameStr = lists:nth(2, RestList),
                    case tryToUpdateUserLocally(Name, GameStr) of
                        ok -> gen_tcp:send(Socket, "OK COOP UPD NAME");
                        error -> gen_tcp:send(Socket, "ERROR COOP UPD NAME")
                    end;
                true -> gen_tcp:send(Socket, "ERROR COOP UPD NAME")
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


%% Process for user purposes

%% Will be created a userSender thread for each user
userSender(Socket) ->
    receive
        {upd, Command} ->
            io:format("here send command ~s~n", [Command]),
            gen_tcp:send(Socket, Command),
            userSender(Socket)
end.

updateInterested(Game, GameStr, Name) ->
    io:format("Here, I have to update ~s ~s and ~p~n",
        [Game#gameState.player1, Game#gameState.player2, Game#gameState.watchers]),
    if
        Game#gameState.player1 /= Name ->
            spawn(?MODULE, resolveUpdate, [Game#gameState.player1, GameStr]);
        true ->
            io:format("~s did the command~n", [Name])
    end,
    if
        Game#gameState.player2 /= Name ->
            spawn(?MODULE, resolveUpdate, [Game#gameState.player2, GameStr]);
        true ->
            io:format("~s did the command~n", [Name])
    end,
    Watchers = lists:filter(fun(X) -> X /= Name end, Game#gameState.watchers),
    io:format("updateInterested ~p~n", [Watchers]),
    lists:foreach(fun(X) ->
        spawn(?MODULE, resolveUpdate, [X, GameStr]) end, Watchers).



tryToUpdateUserLocally(Player, Command) ->
    io:format("tryToUpdateUserLocally ~s~s~n", [Player, Command]),
    GameListPid = global:whereis_name("name_list"),
    GameListPid!{contains, Player, self()},
    receive
        true ->
            Player1Pid = global:whereis_name(Player),
            Player1Pid!{upd, Command},
            ok;
        false -> error
    end.

resolveUpdate(Player, Command) ->
        case tryToUpdateUserLocally(Player, Command) of
            error ->
                io:format("are we in this place?~n"),
                CoopResolveUpdPid = global:whereis_name("cooperative_resolve_upd"),
                CoopResolveUpdPid!{upd, Command, Player},
                ok;
            ok -> ok
    end.

cooperativeResolveUpdate(Ip, Port) ->
    receive
        {upd, Command, Name} ->
            Servers = serverList(Ip, Port),
            io:format("servers ~p~n",[Servers]),
            spawn(?MODULE, resolveUpdateReq, [Servers, Name, Command])
    end,
    cooperativeResolveUpdate(Ip, Port).

resolveUpdateReq(Servers, Name, GameStr) ->
    case Servers of
        [Server | Rest] ->
            Ip = Server#url.ip,
            Port = Server#url.port,
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active,false}, {packet,0}]),
            gen_tcp:send(Socket, "COOP UPD NAME " ++ (Name ++ (" " ++ GameStr))),
            case gen_tcp:recv(Socket, 0) of
                {ok, "ERROR COOP UPD NAME"} ->
                    gen_tcp:close(Socket),
                    resolveUpdateReq(Rest, Name, GameStr);
                {ok, "OK COOP UPD NAME"} ->
                    gen_tcp:close(Socket),
                    true;
                _ ->
                    gen_tcp:close(Socket),
                    error
            end;
        [] -> false
    end.

attendToCoopUpdate(Player, Command) ->
    GameListPid = global:whereis_name("name_list"),
    GameListPid!{contains, Player, self()},
    receive
        true ->
            PlayerPid = global:whereis_name(Player),
            PlayerPid!{upd, Command};
        false ->
            io:format("name ~s is not in this server~n", [Player])
    end.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
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

%%%%%%%%%%%%%%%%%%%%%%%%%%%

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
            GameCounterPid = global:whereis_name("game_counter"),
            GameCounterPid!{add, Name, self(), Pid},
            gameList(GameList);
        {new, Name, GameId, Pid} ->
            Pid!{ok, GameId},
            Game = #gameState{
                id = GameId,
                player1 = Name,
                watchers = []
            },
            gameList(GameList ++ [Game]);
        {get, GameId, Pid} ->
            case searchGame(GameId, GameList) of
                {ok, Game} ->
                    Pid!{ok, Game},
                    gameList(GameList);
                {error, Reason} ->
                    io:format("Error in get game ~s~n", [Reason]),
                    Pid!{error, "Game not found"},
                    gameList(GameList)
            end;

        %%%%%%%%%%%%%
        %% obtains list of games (not blocking)
        {get, Pid} ->
            CoopGameListPid = global:whereis_name("cooperative_game_list"),
            CoopGameListPid!{get, Pid, self()},
            gameList(GameList);
        {getCoop, Pid} ->
            L = parseGameList(GameList),
            Pid!L,
            gameList(GameList);
        %%%%%%%%%%%%

        {gameList, PCommandPid, Games} ->
            TotalListGames = (parseGameList(GameList)) ++ Games,
            PCommandPid!TotalListGames,
            gameList(GameList);
        {update, UpdGame} ->
            {ok, NewGameList} = replaceUpdatedGame(GameList, UpdGame),
            gameList(NewGameList);
        {update, UpdGame, Pid} ->
            case replaceUpdatedGame(GameList, UpdGame) of
                {ok, NewGameList} ->
                    Pid!ok,
                    gameList(NewGameList);
                {error, _} ->
                    Pid!error,
                    gameList(GameList)
            end
    end.

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
            WatchersStr = Game#gameState.watchers,
            Watchers = " Watchers: " ++ string:join(WatchersStr, ", "),
            StrGame = ("\n" ++ (Id ++ (P1 ++ (P2 ++ (Watchers))))),
            parseGameList(Rest, StrGame ++ (Accumulator));
        [] -> Accumulator
    end.


%% receive a list of games and a game, replaces it and returns a new list
replaceUpdatedGame(GameList, UpdGame) ->
    replaceUpdatedGame([], GameList, UpdGame).

replaceUpdatedGame(Front, Back, UpdGame) ->
    io:format("replaceUpdatedGame~n"),
    case Back of
        [Game | Rest] ->
            if
                Game#gameState.id == UpdGame#gameState.id ->
                    io:format("replacing ~s with player ~s~n", [
                        UpdGame#gameState.id, UpdGame#gameState.player2
                    ]),
                    {ok, Front ++ ([UpdGame] ++ Rest)};
                true -> replaceUpdatedGame(Front ++ [Game], Rest, UpdGame)
            end;
        [] -> {error, "game not found"}
    end.
%%%%%%%%%%%%

%%%%%%%
%% get list of games from any servers
cooperativeGameListReq(Port) ->
    cooperativeGameListReq('localhost', Port).


cooperativeGameListReq(Ip, Port) ->
    receive
        {get, PCommandPid, Pid} ->
            Servers = serverList(Ip, Port),
            spawn(?MODULE, gameListReq, [Servers, PCommandPid, Pid]);
        {getGame, GameId, Pid} ->
            io:format("getGame ~s~n", [GameId]),
            Servers = serverList(Ip, Port),
            spawn(?MODULE, getGameReq, [Servers, GameId, Pid]);
        {update, Game} ->
            io:format("updateGame~n"),
            Servers = serverList(Ip, Port),
            spawn(?MODULE, updateGameCoop, [Game, Servers])
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

%% Request to servers for GameId game
getGameReq(Servers, GameId, Pid) ->
    case Servers of
        [Server | Rest] ->
            Ip = Server#url.ip,
            Port = Server#url.port,
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active,false}, {packet,0}]),
            gen_tcp:send(Socket, "COOP GET " ++ GameId),
            case gen_tcp:recv(Socket, 0) of
                {ok, "OK COOP GET " ++ L} ->
                    gen_tcp:close(Socket),
                    Pid!{ok, L};
                {ok, "ERROR COOP GET"} ->
                    gen_tcp:close(Socket),
                    getGameReq(Rest, GameId, Pid);
                _ ->
                    gen_tcp:close(Socket),
                    error
            end;
        [] -> Pid!{error, "Game not found"}
    end.

updateGameCoop(Game, Servers) ->
    GameStr = gameStateToStr(Game),
    updateGameStrCoop(GameStr, Servers).

updateGameStrCoop(Game, Servers) ->
    case Servers of
        [Server | Rest] ->
            Ip = Server#url.ip,
            Port = Server#url.port,
            {ok, Socket} = gen_tcp:connect(Ip, Port, [{active,false}, {packet,0}]),
            gen_tcp:send(Socket, "COOP UPD GAME " ++ Game),
            case gen_tcp:recv(Socket, 0) of
                {ok, "OK COOP UPD GAME"} ->
                    gen_tcp:close(Socket);
                {ok, "ERROR COOP UPD GAME"} ->
                    gen_tcp:close(Socket),
                    updateGameStrCoop(Game, Rest);
                _ ->
                    gen_tcp:close(Socket),
                    error
            end;
        [] -> io:format("game not found~n")
    end.



%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%
%% Modify games
%% Try to add a player in a game

addPlayerAvailable(Name, GameId) ->
    Fun = fun(X, Y) -> tryUpdatePlayer2(X,Y) end,
    changeGameStatus(GameId, Fun, Name).

addWatcher(Name, GameId) ->
    Fun = fun(X, Y) -> tryAddWatcher(X, Y) end,
    changeGameStatus(GameId, Fun, Name).

leaveGame(Name, GameId) ->
    Fun = fun(X, Y) -> tryLeaveGame(X, Y) end,
    changeGameStatus(GameId, Fun, Name).

changeGameStatus(GameId, Fun, Change) ->
    GameListPid = global:whereis_name("game_list"),
    GameListPid!{get, GameId, self()},
    receive
        {ok, Game} ->
            case Fun(Game, Change) of
                error -> error;
                UpdGame ->
                    io:format("receive aupdated game ~s~s~n",[UpdGame#gameState.id, UpdGame#gameState.player2]),
                    GameListPid!{update, UpdGame},
                    GameStr = gameStateToStr(UpdGame),
                    io:format("ya me voy~n"),
                    {ok, UpdGame, GameStr}
            end;
        {error, Reason} ->
            CoopGameListPid = global:whereis_name("cooperative_game_list"),
            CoopGameListPid!{getGame, GameId, self()},
            receive
                {ok, GameStr} ->
                    io:format("receive ~s~n", [GameStr]),
                    case strToGameState(GameStr) of
                        error -> io:format("bad record received~n");
                        Game ->
                            case Fun(Game, Change) of
                                error -> error;
                                UpdGame ->
                                    io:format("update remotelly~n"),
                                    CoopGameListPid!{update, UpdGame},
                                    UpdGameStr = gameStateToStr(UpdGame),
                                    {ok, UpdGame, UpdGameStr}
                            end
                    end;
                {error, Reason} ->
                    io:format("Game ~s not found~n", [GameId])
            end
    end.

tryUpdatePlayer2(Game, Name) ->
    io:format("tryUpdatePlayer2 ~s ~s~n", [Game#gameState.id, Name]),
    case Game#gameState.player2 of
        "-" ->
            io:format("case 1 bro~n"),
            #gameState{
                id = Game#gameState.id,
                player1 = Game#gameState.player1,
                player2 = Name,
                watchers = Game#gameState.watchers
            };
        _ ->
            io:format("Game has already occupied~n"),
            error
    end.

tryAddWatcher(Game, Name) ->
    io:format("addWatcher ~s ~s ~p~n", [Game#gameState.id, Name, Game#gameState.watchers]),
    case lists:member(Name, Game#gameState.watchers) of
        false ->
            io:format("Adding watcher!~n"),
            #gameState{
                id = Game#gameState.id,
                player1 = Game#gameState.player1,
                player2 = Game#gameState.player2,
                watchers = Game#gameState.watchers ++ [Name]
            };
        _ ->
            io:format("~s has already added to watchers~n", [Name]),
            error
    end.


tryLeaveGame(Game, Name) ->
    io:format("tryLeaveGame ~s ~s ~p~n", [Game#gameState.id, Name, Game#gameState.watchers]),
    case lists:member(Name, Game#gameState.watchers) of
        true ->
            #gameState{
                id = Game#gameState.id,
                player1 = Game#gameState.player1,
                player2 = Game#gameState.player2,
                watchers = lists:delete(Name, Game#gameState.watchers)
            };
        _ ->
            io:format("Player ~s is not in watcher list of game ~s~n", [Name, Game#gameState.id]),
            error
    end.

searchGame(GameId, GameList) ->
    case GameList of
        [Game | Rest] ->
            if
                Game#gameState.id == GameId ->
                    {ok, Game};
                true ->
                    searchGame(GameId, Rest)
            end;
        [] ->
            {error, "Game does not exist"}
    end.


gameStateToStr(Game) ->
    Id = Game#gameState.id,
    P1 = " " ++ Game#gameState.player1,
    case Game#gameState.player2 of
        "" -> P2 = " -";
        Val -> P2 = " " ++ Val
    end,
    if
        length(Game#gameState.watchers) > 0 ->
            io:format("gameStateToStr ~p~n", [Game#gameState.watchers]),
            Watchers = " <" ++ (string:join(Game#gameState.watchers, ",") ++  ">");
            true -> Watchers = " <>"
    end,
    GameStr = Id ++ (P1 ++ (P2 ++ (Watchers))),
    io:format("gameStateToStr ~s~n", [GameStr]),
    GameStr.

parseStrWatchersToList(StrWatchers) ->
    if
        length(StrWatchers) > 2 ->
            WStrTemp = tl(StrWatchers),
            WStrTemp_ = string:substr(WStrTemp, 1, length(WStrTemp) - 1),
            Watchers = string:split(WStrTemp_, ","),
            io:format("result: ~p~n", [Watchers]),
            Watchers;
        true -> []
    end.



strToGameState(StrGame) ->
    ListStrGame = string:split(StrGame, " ", all),
    case length(ListStrGame) of
        4 ->
            Id = lists:nth(1, ListStrGame),
            P1 = lists:nth(2, ListStrGame),
            P2 = lists:nth(3, ListStrGame),
            Watchers = parseStrWatchersToList(lists:nth(4, ListStrGame)),
            Game = #gameState{
                id = Id,
                player1 = P1,
                player2 = P2,
                watchers = Watchers,
                state = {}
            },
            Game;
        _ ->
            io:format("Bad string ~s~n", [ListStrGame]),
            #gameState{}
    end.