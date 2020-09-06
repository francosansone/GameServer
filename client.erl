-module(client).
-compile(export_all).

init(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    spawn(?MODULE, controller, [Socket, self()]),
    spawn(?MODULE, read_cmd, [self()]),
    io:format("Enter command: "),
    send_data(Socket).

init() -> init('localhost', 8000).

init(Port) -> init('localhost', Port).

send_data(Socket) ->
    receive
        {newCommand, "a"} ->
            send_data(Socket);
        {newCommand, Cmd} ->
            send(Socket, Cmd),
            send_data(Socket);
        {userName, NewName} ->
            io:format("Update user name ~s~n", [NewName]),
            send_data(Socket);
        ok ->
             send_data(Socket);
        _ ->
            exit(1)
    end.

read_stdin() ->
    case io:get_line("") of
        eof ->
            io:format("eof? ~n");
        "\n" ->
            {ok, ""};
        Line->
            Res = string:sub_string(Line, 1, string:len(Line) - 1),
            % aca una funcion filter, para filtrar comandos
            {ok, Res}
    end.

read_cmd(Pid) ->
    {ok, Cmd} = read_stdin(),
    Pid!{newCommand, Cmd},
    read_cmd(Pid).

read_args() ->
    io:format("Enter arguments: "),
    {ok, Args} = read_stdin(),
    Args.

send(Socket, Data) ->
    io:format("sending ~s~n", [Data]),
    gen_tcp:send(Socket, Data).


controller(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            case Command of
                "OK CON " ++ Name ->
                    Pid!{userName, Name};
                "OK PLAY " ++ GameStr ->
                    print_game(GameStr),
                    Pid!ok;
                "UPD " ++ GameStr ->
                    print_game(GameStr),
                    Pid!ok;
                "OK ACC " ++ GameStr ->
                    print_game(GameStr),
                    Pid!ok;
                "OK OBS " ++ GameStr ->
                    print_game(GameStr),
                    Pid!ok;
                "OK BYE" ->
                    gen_tcp:close(Socket),
                    exit(Pid, "closed");
                Response ->
                    io:format("llego algo! ~s~n", [Response]),
                    Pid!ok
            end,
            io:format("Enter command: "),
            controller(Socket, Pid);
        {error, Reason} ->
            io:format("Error ~p~n", [Reason]),
            gen_tcp:close(Socket),
            exit(Pid, "closed");
        _ ->
            io:format("otra cosa~n"),
            exit(Pid, "closed")
    end.

print_game(GameStr) ->
    %server0_1 fran fren <> 1 0,0,0,0,0,0,0,0,0 1

    GameList = string:split(GameStr, " ", all),
    io:format("~n********************~n"),
    io:format("Room: ~s~n", [lists:nth(1, GameList)]),
    Player1 = lists:nth(2, GameList),
    Player2 = lists:nth(3, GameList),
    io:format("Player 1: ~s~n", [Player1]),
    io:format("Player 2: ~s~n", [Player2]),
    io:format("Watchers: ~s~n", [lists:nth(4, GameList)]),
    Turn = lists:nth(5, GameList),
    if
        length(GameList) == 7 andalso Turn == "1"-> io:format("Winner: ~s~n", [Player2]);
        length(GameList) == 7 -> io:format("Winner: ~s~n", [Player1]);
        Turn == "1" -> io:format("Turn: ~s~n", [Player1]);
        true -> io:format("Turn: ~s~n", [Player2])
    end,

    GameStateStr = lists:nth(6, GameList),
    GameState = lists:sublist(string:split(GameStateStr, ",", all), 9),
    FirstRow = lists:sublist(GameState, 3),
    SecondRow = lists:sublist(GameState, 4, 3),
    ThirdRow = lists:sublist(GameState, 7, 3),
    print_row(FirstRow),
    io:format("---------------~n"),
    print_row(SecondRow),
    io:format("---------------~n"),
    print_row(ThirdRow),
    io:format("********************~n").

print_row(Row) ->
    PrintRow = lists:map(fun(X) ->
        case X of
            "1" -> "O";
            "2" -> "X";
            _ -> " "
        end
    end, Row),
    io:format("~s", [lists:nth(1, PrintRow)]),
    io:format(" | "),
    io:format("~s", [lists:nth(2, PrintRow)]),
    io:format(" | "),
    io:format("~s~n", [lists:nth(3, PrintRow)]).
