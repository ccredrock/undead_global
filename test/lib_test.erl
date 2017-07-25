-module(lib_test).

-export([test/0]).

test() ->
    {ok, spawn(fun() -> receive _ -> ok end end)}.

