-module(client).
-compile(export_all).

init(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    Prompt = spawn(?MODULE, prompt, []),
    spawn(?MODULE, controller, [Socket, self(), Prompt]),
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
    Args = read_args(),
    Line = Cmd ++ (" " ++ Args),
    send(Socket, Line),
    receive
        {userName, NewName} ->
            io:format("Update user name ~s~n", [NewName]),
            send_data(Socket, NewName);
        ok ->
             send_data(Socket);
        _ ->
            exit(1)
    end.


send_data(Socket, UserName) ->
    Cmd = read_cmd(),
    Args = read_args(),
    Line = Cmd ++ ( " " ++ (UserName ++ (" " ++ Args))),
    send(Socket, Line),
    receive
        ok ->
            io:format("ok~n"),
             send_data(Socket, UserName);
        _ ->
            io:format("no ok~n"),
            send_data(Socket, UserName)
    end.

read_stdin() ->
    case io:get_line("") of
        eof ->
            init:stop();
        "\n" ->
            {ok, ""};
        Line->
            Res = string:sub_string(Line, 1, string:len(Line) - 1),
            % aca una funcion filter, para filtrar comandos
            {ok, Res}
    end.

read_cmd() ->
    io:format("Enter command: "),
    {ok, Cmd} = read_stdin(),
    Cmd.

read_args() ->
    io:format("Enter arguments: "),
    {ok, Args} = read_stdin(),
    Args.

send(Socket, Data) ->
    io:format("sending ~s~n", [Data]),
    gen_tcp:send(Socket, Data).


controller(Socket, Pid, Prompt) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            case Command of
                "OK CON " ++ Name ->
                    Pid!{userName, Name},
                    Prompt!{upd, "update bro~n"};
                _ ->
                    Pid!ok
            end,
            controller(Socket, Pid, Prompt);
        {error, Reason} ->
            io:format("Error ~p~n", [Reason]),
            gen_tcp:close(Socket),
            exit(Pid, "closed");
        _ ->
            io:format("otra cosa~n"),
            exit(Pid, "closed")
    end.
