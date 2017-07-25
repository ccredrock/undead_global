-module(undead_global_tests).

-include_lib("eunit/include/eunit.hrl").

-define(Setup, fun() -> application:start(undead_global) end).
-define(Clearnup, fun(_) -> application:stop(undead_global) end).

basic_test_() ->
    {inorder,
     {setup, ?Setup, ?Clearnup,
      [{"exit",
        fun() ->
                ?assertEqual(ok, undead_global:reg(test, {lib_test, test, []})),
                ?assertEqual(1, length(undead_global:get())),
                PID = global:whereis_name(test),
                ?assert(PID =/= undefined),
                exit(PID, kill),
                ?assert(global:whereis_name(test) =:= undefined),
                timer:sleep(1000),
                ?assert(global:whereis_name(test) =/= undefined)
        end}
      ]}
    }.

