-module(client).
-compile(export_all).

init(Host, Port) ->
    {ok, Socket} = gen_tcp:connect(Host, Port, [{active,false}, {packet,0}]),
    spawn(?MODULE, controller, [Socket, self()]),
    send_data(Socket, "").


send_data(Socket, Name) ->
    {ok, Line} = read_stdin(),
    io:format("~s~n", [Line]),
    send(Socket, Name ++ "-" ++ Line),
    receive
        {userName, NewName} ->
            io:format("Update user name ~s~n", [NewName]),
            send_data(Socket, NewName);
        ok ->
             send_data(Socket, Name);
        _ ->
            exit(1)
    end.

read_stdin() ->
    io:format("Enter command: "),
    case io:get_line("") of
        eof ->
            init:stop();
        "\n" ->
            io:format("Bad value~n"),
            read_stdin();
        Line->
            Res = string:sub_string(Line, 1, string:len(Line) - 1),
            % aca una funcion filter, para filtrar comandos
            {ok, Res}
    end.


send(Socket, Data) ->
    gen_tcp:send(Socket, Data).


controller(Socket, Pid) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Command} ->
            case Command of
                "OK CON " ++ Name ->
                    io:format("new name!"),
                    Pid!{userName, Name};
                _ ->
                    Pid!ok
            end,
            controller(Socket, Pid);
        {error, Reason} ->
            io:format("Error ~p~n", [Reason]),
            gen_tcp:close(Socket),
            exit(Pid, "closed");
        _ ->
            io:format("otra cosa~n"),
            exit(Pid, "closed")
    end.
