-module(server).
-compile(export_all).


-record(gameState, {
    id="",
    player1="-",
    player2="-",
    watchers=[],
    turn=1,
    state=[],
    node=undefined,
    victory=0
}).

-record(url, {id="server0", ip='localhost', port=8000, queue=0, socket="", isLocal=false}).

server(Port) ->
    {ok, ListenSocket} = gen_tcp:listen(Port, [{active,false}, {packet,0}]),
    io:format("Listening in port ~p~n", [Port]),
    initControllers(Port),
    io:format("finish initcontroller~n"),
    dispatcher(ListenSocket, Port).

initControllers(Port) ->
    UserNamesPid = spawn(?MODULE, userNames, [[]]),
    GameListPid = spawn(?MODULE, game_list, [[]]),
    GameCounterPid = spawn(?MODULE, gameCounter, [Port]),
    Nodes = lists:map(fun(X) -> {X, 0} end, node_names()),
    PBalance = spawn(?MODULE, pbalance, [Nodes]),
    NodeName = my_node_name(Port),
    io:format("node name ~p~n", [NodeName]),
    net_kernel:start([NodeName, shortnames]),

    %%connect to rest of nodes
    sleep(5000),
    lists:foreach(fun(X) ->
                    net_kernel:connect_node(X)
                  end, node_names()
    ),
    register(pbalance, PBalance),
    register(name_list, UserNamesPid),
    register(game_list, GameListPid),
    register(game_counter, GameCounterPid),
    spawn(?MODULE, pstat, []).

sleep(Time) ->
    receive
        after Time ->
            true
        end.

node_names() -> ['server0@localhost', 'server1@localhost'].

my_node_name(Port) ->
    case Port of
        8000 -> server0@localhost;
        8001 -> server1@localhost
    end.

dispatcher(ListenSocket, Port) ->
    io:format("dispatcher~n"),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    UserPid = spawn(?MODULE, userSender, [Socket]),
    spawn(?MODULE, psocket, [Socket, UserPid, node()]),
    dispatcher(ListenSocket, Port).

pstat() ->
    sleep(5000),
    Tasks = erlang:statistics(total_active_tasks),
    lists:foreach(fun(Node) ->
        {pbalance, Node}!{stat, node(), Tasks}
                  end, node_names()),
    pstat().

pbalance(ServerList) ->
    receive
        {stat, Node, Tasks} ->
            NewServerList = lists:map(fun(X) ->
                if
                    element(1, X) == Node -> {Node, Tasks};
                    true -> X
                end
            end, ServerList),
            pbalance(NewServerList);
        {lessTasks, Pid} ->
            P = lists:last(ServerList),
            PP = {element(1, P), element(2, P) + 50},
            LessLoaded = lists:foldl(fun(X, Node) ->
                if
                    element(2, X) < element(2, Node) -> Node;
                    true -> Node
                end
            end, PP, lists:droplast(ServerList)),
            Pid!element(1, LessLoaded),
            pbalance(ServerList)
    end.

psocket(Socket, UserPid, NodeName) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            case Command of
                "CON " ++ _Name ->
                    spawn(?MODULE, pcommand, [Command, UserPid]);
                _ ->
                    io:format("psocket ~s~n", [Command]),
                    PBalance = whereis(pbalance),
                    PBalance!{lessTasks, self()},
                    receive
                        Node ->
                            io:format("less loaded ~p~n", [Node]),
                            spawn(Node, ?MODULE, pcommand, [Command, UserPid])
                    after
                        5000 ->
                            io:format("pbalance does not response~n"),
                            spawn(?MODULE, pcommand, [Command, UserPid, NodeName])
                    end
            end,
            psocket(Socket, UserPid, NodeName);

        {error, Reason} ->
            io:format("closed ~p~n",[Reason]),
            gen_tcp:close(Socket)
    end.

pcommand(Command, UserPid) -> pcommand(Command, UserPid, "").

pcommand(Command, UserPid, Node) ->
    io:format("pcommand ~s~n", [Command]),
    case Command of
        "CON " ++ Name ->
            io:format("Recibe nombre ~s~n", [Name]),
            case checkIfNameExists(Name) of
                false ->
                    io:format("ok ~p~n", [Node]),
                    addName(Name),
                    UserPid!{register_name, Name},
                    UserPid!{upd, "OK " ++ ("CON " ++ Name)};
                _ ->
                    io:format("error~n"),
                    UserPid!{upd, "ERROR " ++ ("CON " ++ Name)}
            end;
        "NEW " ++ Name ->
            io:format("~s: ~s~n", [Name, Command]),
            %% First, check if name exists
            case checkIfNameExists(Name) of
                true ->
                    GameListPid = whereis(game_list),
                    GameListPid!{add, Name, self()},
                    receive
                        {ok, GameId} ->
                            UserPid!{upd, "OK NEW " ++ GameId};
                            %gen_tcp:send(Socket, "OK NEW " ++ GameId);
                        _ -> UserPid!{upd, "ERROR NEW"}% gen_tcp:send(Socket, "ERROR NEW")
                    end;
                _ -> {UserPid, Node}!{upd,  "ERROR NEW"}
            end;
        "LSG " ++ Name ->
            io:format("LSG ~s ~n", [Name]),
            UserPid!{upd, "OK LSG " ++ get_game_list()};
        "ACC " ++ (Rest) ->
            io:format("ACC ~s~n", [Rest]),
            Rest_ = string:split(Rest, " "),
            case length(Rest_) of
                2 ->
                    Name = lists:nth(1, Rest_),
                    GameId = lists:nth(2, Rest_),
                    case checkIfNameExists(Name) of
                        false -> UserPid!{upd, "ERROR ACC"};
                        true ->
                            case addPlayerAvailable(Name, GameId) of
                                {ok, Game, GameStr} ->
                                    UserPid!{upd, "OK ACC " ++ GameStr},
                                    updateInterested(Game, GameStr, Name);
                                {error, Reason} -> UserPid!{upd, "ERROR ACC " ++ Reason};
                                error -> UserPid!{upd, "ERROR ACC"}
                            end
                    end;
                _ ->
                    io:format("Bad command~n"),
                    UserPid!{upd, "ERROR ACC"}
            end;
        "OBS " ++ (Rest) ->
            io:format("OBS ~s~n", [Rest]),
            Rest_ = string:split(Rest, " "),
            case length(Rest_) of
                2 ->
                    Name = lists:nth(1, Rest_),
                    GameId = lists:nth(2, Rest_),
                    case checkIfNameExists(Name) of
                        false -> UserPid!{upd, "ERROR OBS"};
                        true ->
                            case addWatcher(Name, GameId) of
                                {ok, Game, GameStr} ->
                                    UserPid!{upd, "OK OBS " ++ GameStr},
                                    updateInterested(Game, GameStr, Name);
                                {error, Reason} -> UserPid!{upd, "ERROR OBS " ++ Reason}
                    end
                end;
                _ ->
                    io:format("Bad command~n"),
                    UserPid!{upd, "ERROR ACC"}
            end;
        "LEA " ++ (Rest) ->
            io:format("OBS ~s~n", [Rest]),
            Rest_ = string:split(Rest, " "),
            case length(Rest_) of
                2 ->
                    Name = lists:nth(1, Rest_),
                    GameId = lists:nth(2, Rest_),
                    case checkIfNameExists(Name) of
                        false -> UserPid!{upd, "ERROR LEA"};
                        true ->
                            case leaveGame(Name, GameId) of
                                {ok, Game, GameStr} ->
                                    UserPid!{upd, "OK LEA"},
                                    updateInterested(Game, GameStr, Name);
                                {error, Reason} -> UserPid!{upd, "ERROR LEA " ++ Reason}
                            end
                    end;
                _ ->
                    io:format("Bad command~n"),
                    UserPid!{upd, "ERROR ACC"}
            end;
        "PLA " ++ Rest ->
            io:format("PLA ~s~n", [Rest]),
            Rest_ = string:split(Rest, " ", all),
            case length(Rest_) of
                3 ->
                    Name = lists:nth(1, Rest_),
                    case checkIfNameExists(Name) of
                        false -> UserPid!{upd, "ERROR PLAY"};
                        true ->
                            GameId = lists:nth(2, Rest_),
                            case parsePlay(lists:nth(3, Rest_)) of
                                error -> UserPid!{upd, "ERROR PLAY"};
                                Play ->
                                    MoveRes = makeAMove({Name, Play}, GameId),
                                    io:format("makeAMove returns ~p~n", [MoveRes]),
                                    case MoveRes of
                                        {ok, Game, GameStr} ->
                                            if
                                                Game#gameState.victory == 1 ->
                                                    GameStrFinal = GameStr ++ " 1";
                                                true ->
                                                    GameStrFinal = GameStr
                                            end,
                                            UserPid!{upd, "OK PLAY " ++ GameStrFinal},
                                            updateInterested(Game, GameStrFinal, Name);
                                        {error, Reason} -> UserPid!{upd, "ERROR PLAY " ++ Reason};
                                        error -> UserPid!{upd, "ERROR PLAY"}
                                    end
                            end
                    end;
                _ -> UserPid!{upd, "ERROR PLA"}
            end;
        "BYE " ++ Name ->
            case checkIfNameExists(Name) of
                true ->
                    case leaveGames(Name) of
                        ok ->
                            removeName(Name),
                            UserPid!{upd, "OK BYE"}
                    end;
                _ ->  UserPid!{upd, "ERROR BYE"}
            end;
        _ ->
            io:format("Unexpected~n"),
            UserPid!{upd, "ERROR No Implementado"}
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
    lists:filter(fun(X)->
        P = X#url.port /= Port,
        I = X#url.ip /= Ip,
        P or I end, L).


%% Just load server
loadserver(N) ->
    if
        N > 0 ->
            io:format("loadserver ~p~n", [N]),
            loadserver(N-1);
        true -> void
    end.

%% Process for user purposes

%% Will be created a userSender thread for each user
userSender(Socket) ->
    receive
        {register_name, Name} ->
            io:format("registering user ~s~n", [Name]),
            register(list_to_atom(Name), self()),
            userSender(Socket);
        {upd, Command} ->
            io:format("USER SENDER CALLING here send command ~s~n", [Command]),
            A = gen_tcp:send(Socket, Command),
            io:format("USER SENDER CALLED here send command ~s ~p~n", [Command, A]),
            userSender(Socket)
    end.

updateInterested(Game, GameStr, Name) ->
    io:format("Here, I have to update ~s ~s and ~p~n",
        [Game#gameState.player1, Game#gameState.player2, Game#gameState.watchers]),
    if
        Game#gameState.player1 /= Name ->
            spawn(?MODULE, send_update, [Game#gameState.player1, "UPD " ++ GameStr]);
        true ->
            io:format("~s did the command~n", [Name])
    end,
    if
        Game#gameState.player2 /= Name ->
            spawn(?MODULE, send_update, [Game#gameState.player2, "UPD " ++ GameStr]);
        true ->
            io:format("~s did the command~n", [Name])
    end,
    Watchers = lists:filter(fun(X) -> X /= Name end, Game#gameState.watchers),
    io:format("updateInterested ~p~n", [Watchers]),
    lists:foreach(fun(X) ->
        spawn(?MODULE, send_update, [X, "UPD " ++ GameStr]) end, Watchers).


send_update(Player, Command) ->
    NodeName = lists:foldl(fun(X, Acc) ->
        case Acc of
            undefined ->
                {name_list, X}!{contains, Player, self()},
                receive
                    true -> X;
                    false -> undefined
                after
                    5000 -> undefined
                end;
            _ -> Acc
        end
    end, undefined, node_names()),
    io:format("send_update ~s ~p~n", [Player, NodeName]),
    case NodeName of
        undefined -> io:format("Error sending update to player ~s~n", [Player]);
        _ -> {list_to_atom(Player), NodeName}!{upd, Command}
    end.



tryToUpdateUserLocally(Player, Command) ->
    io:format("tryToUpdateUserLocally ~s ~s~n", [Player, Command]),
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

resolveUpdateReq(Servers, Name, GameStr) ->
    case Servers of
        [Server | Rest] ->
            io:format("trying ~s ~p~n", [Name, Server]),
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
        {add, Name} ->
            io:format("adding name ~s~n",[Name]),
            userNames(UsernameList ++ [Name]);
        {contains, Name, Pid} ->
            io:format("here we are: ~p~n", [lists:member(Name, UsernameList)]),
            Pid!lists:member(Name, UsernameList),
            userNames(UsernameList);
        {remove, Name} ->
                NewList =lists:filter(fun(X) -> X /= Name end, UsernameList),
                userNames(NewList)
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
    io:format("checkIfNameExists~n"),
    lists:foldl(
        fun(X, Acc) ->
            if
                Acc -> Acc;
                true ->
                    {name_list, X}!{contains, Name, self()},
                        receive
                            Val -> Val
                        after
                            5000 -> false
                        end
            end
        end, false, node_names()).

addName(Name) ->
    NameList = whereis(name_list),
    NameList!{add, Name}.

removeName(Name) ->
    UserNamesPid = global:whereis_name("name_list"),
    UserNamesPid!{remove, Name},
    removeNameReq(totalServerList(), Name).

removeNameReq(Servers, Name) ->
    lists:foreach(fun(X) ->
        if
            not( X#url.isLocal ) ->
                {ok, Socket} = gen_tcp:connect(X#url.ip, X#url.port, [{active,false}, {packet,0}]),
                gen_tcp:send(Socket, "COOP REMOVE NAME " ++ Name);
            true -> skip
        end
    end, Servers).

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

game_list(GameList) ->
    receive
        {add, Name, Pid} ->
            GameCounterPid = erlang:whereis(game_counter),
            GameCounterPid!{add, Name, self(), Pid},
            game_list(GameList);
        {new, Name, GameId, Pid} ->
            Pid!{ok, GameId},
            Game = #gameState{
                id = GameId,
                player1 = Name,
                watchers = [],
                node=node()
            },
            game_list(GameList ++ [Game]);
        {get, GameId, Pid} ->
            case searchGame(GameId, GameList) of
                {ok, Game} ->
                    Pid!{ok, Game},
                    game_list(GameList);
                {error, Reason} ->
                    io:format("Error in get game ~s~n", [Reason]),
                    Pid!{error, Reason},
                    game_list(GameList)
            end;

        %%%%%%%%%%%%%
        %% obtains list of games (not blocking)
        {get, Pid} ->
            Pid!GameList,
            game_list(GameList);
        {getCoop, Pid} ->
            L = parseGameList(GameList),
            Pid!L,
            game_list(GameList);
        %%%%%%%%%%%%

        {gameList, PCommandPid, Games} ->
            TotalListGames = (parseGameList(GameList)) ++ Games,
            PCommandPid!TotalListGames,
            game_list(GameList);
        {update, UpdGame} ->
            game_list(replaceUpdatedGame(GameList, UpdGame))
    end.

get_game_list() ->
    Games = lists:foldl(fun(X, GameList) ->
        {game_list, X}!{get, self()},
        receive
            Games -> lists:append(Games, GameList)
        after
            5000 -> GameList
        end
    end, [], node_names()),
    parseGameList(Games).


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
    if
        UpdGame#gameState.victory == 1 ->
            removeGame(GameList, UpdGame);
        (UpdGame#gameState.player1 == "-") and  (UpdGame#gameState.player2 == "-")->
            removeGame(GameList, UpdGame);
        true->
            lists:map(fun(Game) ->
                if
                    Game#gameState.id == UpdGame#gameState.id ->
                        io:format("replacing ~s with player ~s~n", [
                            UpdGame#gameState.id, UpdGame#gameState.player2
                        ]),
                        UpdGame;
                    true -> Game
                end
            end, GameList)
    end.

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

removeGame(GameList, UpdGame) ->
    io:format("removing game ~n"),
    L = lists:filter(fun(Game) -> Game#gameState.id =/= UpdGame#gameState.id end, GameList),
    {ok, L}.
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
            io:format("updateGame ~p~n", [Game]),
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
    Fun = fun(X, Y) -> tryUpdatePlayer(X,Y) end,
    changeGameStatus(GameId, Fun, Name).

addWatcher(Name, GameId) ->
    Fun = fun(X, Y) -> tryAddWatcher(X, Y) end,
    changeGameStatus(GameId, Fun, Name).

leaveGame(Name, GameId) ->
    Fun = fun(X, Y) -> tryLeaveGame(X, Y) end,
    changeGameStatus(GameId, Fun, Name).

makeAMove(Change, GameId) ->
    Fun = fun(X,Y) -> tryToPlay(X, Y) end,
    changeGameStatus(GameId, Fun, Change).

%% bye command
leaveGames(Name) ->
    GameListPid = global:whereis_name("game_list"),
    GameListPid!{get, self()},
    receive
        Games ->
            GamesList = string:split(Games, "\n", all),
            GamesList_ = lists:filter(fun(X) -> not string:is_empty(X) end, GamesList),
            GamesList__ = lists:map(fun(X) -> string:split(X, " ", all) end, GamesList_),
            GamesList___ = lists:filter(fun(X) -> length(X) > 2 end, GamesList__),
            GameIdList = lists:map(fun(X) -> lists:nth(2, X) end, GamesList___),
            lists:foreach(fun(X) -> leaveGame(Name, X) end, GameIdList)
    end.

leaveGamesRecursive(Name, [Game | Rest]) ->
    leaveGame(Name, Game#gameState.id),
    leaveGamesRecursive(Name, Rest);
leaveGamesRecursive(_, []) -> ok.

changeGameStatus(GameId, Fun, Change) ->
    FindGame = lists:foldl(fun(X, Acc) ->
        case Acc of
            undefined ->
                {game_list, X}!{get, GameId, self()},
                receive
                    {ok, Game} -> {ok, Game};
                    {error, _Reason} -> undefined
                end;
            GameFound -> GameFound
        end
    end, undefined, node_names()),
    case FindGame of
        undefined -> {error, "Game not found"};
        {ok, Game} ->
            case Fun(Game, Change) of
                error -> error;
                {error, _Reason} -> io:format("ERROR ~p~n", [_Reason]), {error, _Reason};
                {ok, UpdGame} ->
                    io:format("update remotelly ~p~n",[UpdGame]),
                    {game_list, UpdGame#gameState.node}!{update, UpdGame},
                    {ok, UpdGame, gameStateToStr(UpdGame)}

            end
    end.

tryUpdatePlayer(Game, Name) ->
    io:format("tryUpdatePlayer2 ~s ~s~n", [Game#gameState.id, Name]),
    case Game#gameState.player2 of
        "-" ->
            io:format("case 1 bro~n"),
            {ok, startGame(Game#gameState{ player2 = Name })};
        _ ->
            case Game#gameState.player1 of
                "-" ->
                    {ok, startGame(Game#gameState{ player1 = Name })};
                _ ->
                    io:format("Game has already occupied~n"),
                    {error, "Game has already occupied"}
            end
    end.

tryAddWatcher(Game, Name) ->
    io:format("addWatcher ~s ~s ~p~n", [Game#gameState.id, Name, Game#gameState.watchers]),
    case lists:member(Name, Game#gameState.watchers) of
        false ->
            io:format("Adding watcher!~n"),
            {ok, Game#gameState{watchers = Game#gameState.watchers ++ [Name]}};
        _ ->
            io:format("~s has already added to watchers~n", [Name]),
            {error, Name ++ " has already added to watchers"}
    end.


tryLeaveGame(Game, Name) ->
    io:format("tryLeaveGame ~s ~s ~p~n", [Game#gameState.id, Name, Game#gameState.watchers]),
    if
        Game#gameState.player1 == Name ->
            {ok, Game#gameState{player1 = "-"}};

        Game#gameState.player2 == Name ->
            {ok, Game#gameState{
                player2 = "-"
            }};

        true ->
            case lists:member(Name, Game#gameState.watchers) of
                true ->
                    {ok, Game#gameState{watchers = lists:delete(Name, Game#gameState.watchers)}};
                _ ->
                    io:format("Player ~s is not in this game ~s~n", [Name, Game#gameState.id]),
                    {error, "Player ~s is not in this game"}
            end
    end.

startGame(Game) ->
    Game#gameState{
        turn = 1,
        state = [0, 0, 0, 0, 0, 0, 0, 0, 0]
    }.

allowedPlay() ->
    [[[1, 1], [1,2]],
        [[1, 1], [2,1]],
        [[1, 1], [2,2]],
        [[1,2], [1,3]],
        [[1,2], [2,2]],
        [[1,3], [2,3]],
        [[1,3], [2,2]],
        [[2,1], [3,1]],
        [[2,1], [2,2]],
        [[2,2], [3,2]],
        [[2,2], [2,3]],
        [[2,3], [3,3]],
        [[1,1], [2,2]],
        [[2,2], [1,3]],
        [[2.2], [3,1]],
        [[3,1], [3,2]],
        [[2,2], [3,3]],
        [[3,2], [3,3]]
    ].

isAllowedPlay1(Play) ->
    io:format("isAllowedPlay1 ~p~n", [Play]),
    lists:foldl(
        fun(X, ACC) ->
            case ACC of
                true -> (X > 0) and (X < 4);
                _ -> false
            end
        end,
        true,
        lists:flatten(Play)
    ).
isAllowedPlay2(Play) ->
    Plays = allowedPlay(),
    lists:foldl(
        fun(X, Acc) ->
            case Acc of
                false ->
                    lists:member(lists:nth(1, Play), X) and lists:member(lists:nth(2, Play), X);
                T -> T
            end
        end,
        false,
        Plays
    ).

tryToPlay(Game, Change) ->
    Name = element(1, Change),
    io:format("tryToPlay ~s ~s ~s~n", [Game#gameState.player1, Game#gameState.player2, Name]),
    Play = element(2, Change),
    %% check player
    if
        Name == Game#gameState.player1 ->
            checkTurn(Play, 1, Game);
        Name == Game#gameState.player2 ->
            checkTurn(Play, 2, Game);
        true -> {error, "Player is not in this game"}
    end.

%% check turn
checkTurn(Play, Player, Game) ->
    io:format("checkTurn Player: ~p Turn: ~p ~n", [Player, Game#gameState.turn]),
    if
        Player == Game#gameState.turn ->
            io:format("calling make a play ~p~n", [Play]),
            case length(Play) of
                1 -> tryToPlay1(Play, Player, Game);
                2 -> tryToPlay2(Play, Player, Game);
                _ -> {error, "UNACCEPTABLE PLAY"}
            end;
        true -> {error, "IT'S NOT YOUR TURN"}
    end.

tryToPlay1(Play, Player, Game) ->
    Play_ = lists:nth(1, Play),
    io:format("tryToPlay1 ~p ~p~n", [Play, Play_]),
    ListValue = 3*(lists:nth(1, Play_) - 1) + lists:nth(2, Play_),
    State = Game#gameState.state,
    io:format("tryToPlay1 ~p ~p ~n", [ListValue, State]),
    case isAllowedPlay1(Play) of
        true ->
            io:format("try to play ~p ~p~n", [ListValue, State]),
            case lists:nth(ListValue, State) == 0 of
                false -> {error, "PLACE IS OCCUPIED"};
                _ ->
                    case count(Player, State) < 3 of
                        false -> {error, "UNACCEPTABLE PLAY"};
                        _ ->
                            NewState = lists:sublist(State, ListValue - 1) ++
                                    [Player] ++ lists:sublist(State, ListValue + 1, 9),
                            NewGame = Game#gameState{
                                state = NewState,
                                turn = case Game#gameState.turn of 1 -> 2; _ -> 1 end,
                                victory = victoryCheck(NewState)
                            },
                            io:format("tryToPlay ~p~n", [NewGame]),
                            {ok, NewGame}
                    end
            end;
        _ -> {error, "UNACCEPTABLE PLAY"}
    end.


tryToPlay2(Play, Player, Game) ->
    io:format("tryToPlay2 ~p ~p~n", [Play, Player]),
    From = lists:nth(1, Play),
    ListFrom = 3*(lists:nth(1, From) - 1) + lists:nth(2, From),
    To = lists:nth(2, Play),
    ListTo = 3*(lists:nth(1, To) - 1) + lists:nth(2, To),
    State = Game#gameState.state,
    case (lists:nth(ListFrom, State) == Player) and (lists:nth(ListTo, State) == 0) of
        false -> {error, "UNACCEPTABLE PLAY"};
        _ -> case isAllowedPlay2(Play) of
                 false -> {error, "UNACCEPTABLE PLAY"};
                 _ ->
                     case listSwap(State, ListFrom, ListTo) of
                         error -> {error, "UNACCEPTABLE PLAY"};
                         NewState ->
                             NewGame = Game#gameState{
                                 state = NewState,
                                 turn = case Game#gameState.turn of 1 -> 2; _ -> 1 end,
                                 victory = victoryCheck(NewState)
                             },
                             io:format("tryToPlay2 ~p~n", [NewGame]),
                             {ok, NewGame}
                     end
             end
    end.


%% Functions to check victory
victoryCheck(State) ->
    case horizontalCheck(State) == 1 orelse verticalCheck(State) == 1 orelse diagonalCheck(State) == 1 of
        true -> 1;
        _ -> 0
    end.

isLine(A, B, C) ->
    if
        (A == B) and (B == C) and (A /= 0) -> A;
        true -> 0
    end.

horizontalCheck([A , B, C | Rest]) ->
    io:format("horizontal check ~p, ~p~n", [[A,B,C], Rest]),
    case isLine(A, B, C) of
        0 -> horizontalCheck(Rest);
        Val -> Val
    end;
horizontalCheck([]) -> 0.

verticalCheck(State) ->
    List = [lists:nth(1, State),
        lists:nth(4, State),
        lists:nth(7, State),
        lists:nth(2, State),
        lists:nth(5, State),
        lists:nth(8, State),
        lists:nth(3, State),
        lists:nth(6, State),
        lists:nth(9, State)],
    io:format("verticalCheck~p~n", [List]),
    horizontalCheck(List).

diagonalCheck(State) ->
    List = [lists:nth(1, State),
        lists:nth(5, State),
        lists:nth(9, State),
        lists:nth(3, State),
        lists:nth(5, State),
        lists:nth(7, State)],
    io:format("diagonalCheck~p~n", [List]),
    horizontalCheck(List).
%%%%%

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
    io:format("gameStateToStr ~p~n", [Game]),
    Id = Game#gameState.id,
    P1 = " " ++ Game#gameState.player1,
    case Game#gameState.player2 of
        "" -> P2 = " -";
        Val -> P2 = " " ++ Val
    end,
    if
        length(Game#gameState.watchers) > 0 ->
            Watchers = " <" ++ (string:join(Game#gameState.watchers, ",") ++  ">");
            true -> Watchers = " <>"
    end,
    Turn = " " ++ integer_to_list(Game#gameState.turn),
    State = " " ++ lists:foldl(fun(X, Acc) ->  Acc ++ lists:flatten(io_lib:format("~p", [X])) ++ "," end,
        "", Game#gameState.state),
    GameStr = Id ++ P1 ++ P2 ++ Watchers ++ Turn ++ State,
    io:format("gameStateToStr ~s ~s ~s~n", [Id, P2, GameStr]),
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
        6 ->
            Id = lists:nth(1, ListStrGame),
            P1 = lists:nth(2, ListStrGame),
            P2 = lists:nth(3, ListStrGame),
            Watchers = parseStrWatchersToList(lists:nth(4, ListStrGame)),
            Turn = list_to_integer(lists:nth(5, ListStrGame)),
            StrState = lists:nth(6, ListStrGame),
            case length(StrState) of
                0 -> State =[];
                _ ->
                    _State = lists:sublist(string:split(StrState, ",", all), 1, 9),
                    io:format("strToGameState ~s ~p~n", [string:split(StrState, ",", all), _State]),
                    State = lists:map(fun(X) -> list_to_integer(X) end, _State),
                    io:format("strToGameState ~s ~p ~n", [StrState, State])
            end,
            Game = #gameState{
                id = Id,
                player1 = P1,
                player2 = P2,
                watchers = Watchers,
                turn = Turn,
                state = State
            },
            Game;
        _ ->
            io:format("Bad string ~s ~p~n", [ListStrGame, length(ListStrGame)]),
            #gameState{}
    end.

parsePlay(Play) ->
    Plays = string:split(Play, "->", all),
    io:format("parsePlay ~p ~p~n", [Plays, length(Plays)]),
    case length(Plays) of
        2 -> parsePlay2(Plays);
        1 -> parsePlay1(Play);
        _ -> error
    end.

parsePlay1(Play) ->
    Places = string:split(Play, ",", all),
    case length(Places) of
        2 ->
            P1 = lists:nth(1, Places),
            P2 = lists:nth(2, Places),
            io:format("parsePlay1 ~p~p~n", [P1, P2]),
            X = toInteger(P1),
            Y = toInteger(P2),
            if
                (X == error) or (Y == error) -> error;
                true -> [[X | [Y]]]
            end;
        _ -> error
    end.

parsePlay2(Play) ->
    From = lists:nth(1, Play),
    To = lists:nth(2, Play),
    io:format("parsePlay2 ~s ~s ~s~n", [Play, From, To]),
    case parsePlay1(From) of
        error ->
            io:format("error caso 1~n"),
            error;
        IntFrom ->
            case parsePlay1(To) of
                error ->
                    io:format("error caso 2~n"),
                    error;
                IntTo -> [lists:nth(1, IntFrom) | [lists:nth(1, IntTo)]]
            end
    end.

toInteger(Value) ->
    io:format("~s~n", [Value]),
    try list_to_integer(Value)
    catch
        _:_ -> error
    end.

count(_, []) -> 0;
count(X, [X|XS]) -> 1 + count(X, XS);
count(X, [_|XS]) -> count(X, XS).

listSwap(List, A, B) ->
    Size = length(List),
    if
        ((A < 1) or (A > Size) or (B < 1) or (B > Size)) ->
            error;
        true ->
            ValueA = lists:nth(A, List),
            ValueB = lists:nth(B, List),
            L1 = lists:sublist(List, A - 1) ++ [ValueB]
                ++ lists:sublist(List, A + 1, Size - A),
            lists:sublist(L1, B - 1) ++ [ValueA]
            ++ lists:sublist(L1, B + 1, Size - B)
    end.
