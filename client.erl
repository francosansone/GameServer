-module(client).
-compile(export_all).

init(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    spawn(?MODULE, controller, [Socket, self()]),
    send_data(Socket).

init() -> init('localhost', 8000).

init(Port) -> init('localhost', Port).

prompt() ->
    receive
        {upd, Upd} ->
            io:format("Game update ~p~n", [Upd]),
            prompt()
    end.


send_data(Socket) ->
    Cmd = read_cmd(),
    %Args = read_args(),
    %Line = Cmd ++ (" " ++ Args),
    send(Socket, Cmd),
    receive
        {userName, NewName} ->
            io:format("Update user name ~s~n", [NewName]),
            send_data(Socket);
        ok ->
             send_data(Socket);
        _ ->
            exit(1)
    end.


%send_data(Socket, UserName) ->
%    Cmd = read_cmd(),
%    Args = read_args(),
%    Line = Cmd ++ ( " " ++ (UserName ++ (" " ++ Args))),
%    send(Socket, Line),
%    receive
%        ok ->
%            io:format("ok~n"),
%             send_data(Socket, UserName);
%        _ ->
%            io:format("no ok~n"),
%            send_data(Socket, UserName)
%    end.

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

read_cmd() ->
    {ok, Cmd} = read_stdin(),
    Cmd.

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
                "GAME FINISHED " ++ GameStr ->
                    io:format("~nGame over!~n"),
                    print_game(GameStr),
                    Pid!ok;
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
    %server0_1 fran fren <> 1 0,0,0,0,0,0,0,0,0
    GameList = string:split(GameStr, " ", all),
    io:format("~n********************~n"),
    io:format("Room name: ~s~n", [lists:nth(1, GameList)]),
    Player1 = lists:nth(2, GameList),
    Player2 = lists:nth(3, GameList),
    io:format("Player 1: ~s~n", [Player1]),
    io:format("Player 2: ~s~n", [Player2]),
    Turn = lists:nth(5, GameList),
    if
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
