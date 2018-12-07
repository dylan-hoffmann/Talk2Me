-module(testerl).

-export([start/0, listen/0]).



start() ->
    io:format("This is a test.~n"),
    P2 = spawn(testerl, listen, []),
    {ok, P1} = python:start([{python_path, "."}, {python, "python3"}]),
    python:call(P1, talk2me, start, [P2]),
    P1.


listen() ->
    receive
        MSG -> io:format(MSG)
    end,
    listen().


