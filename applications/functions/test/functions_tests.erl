-module(functions_tests).

-include_lib("eunit/include/eunit.hrl").
-include("functions.hrl").

func_init_module_test() ->
    lager:start(),
    ?assertEqual(ok, functions_init:init_modules()),
    Modules = functions_init:existing_modules(),
    ?assertEqual([functions_tests], Modules).