-module(pqc_kzs_cache).
-behaviour(proper_statem).

-export([command/1
        ,initial_state/0
        ,next_state/3
        ,postcondition/3
        ,precondition/2

        ,correct/0
        ,correct_parallel/0

        ,create/0, update/1, get/1
        ,run_counterexample/0, run_counterexample/1
        ]).

-include_lib("proper/include/proper.hrl").

-define(DB, <<?MODULE_STRING>>).
-define(ID, <<?MODULE_STRING>>).

-spec correct() -> any().
correct() ->
    ?FORALL(Cmds
           ,commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   kz_datamgr:flush_cache_docs(?DB),
                   kz_datamgr:db_create(?DB),
                   kz_datamgr:del_doc(?DB, ?ID),
                   {History, State, Result} = run_commands(?MODULE, Cmds),
                   kz_datamgr:flush_cache_docs(?DB),
                   kz_datamgr:del_doc(?DB, ?ID),
                   ?WHENFAIL(io:format("Final State: ~p\nFailing Cmds: ~p\n"
                                      ,[State, zip(Cmds, History)]
                                      )
                            ,aggregate(command_names(Cmds), Result =:= 'ok')
                            )
               end
              )
           ).

-spec correct_parallel() -> any().
correct_parallel() ->
    ?FORALL(Cmds
           ,parallel_commands(?MODULE)
           ,?TRAPEXIT(
               begin
                   kz_datamgr:flush_cache_docs(?DB),
                   kz_datamgr:db_create(?DB),
                   _ = kz_datamgr:del_doc(?DB, ?ID),
                   {History, State, Result} = run_parallel_commands(?MODULE, Cmds),
                   kz_datamgr:flush_cache_docs(?DB),
                   _ = kz_datamgr:del_doc(?DB, ?ID),
                   ?WHENFAIL(io:format("=======~n"
                                       "Failing command sequence:~n~p~n"
                                       "At state: ~p~n"
                                       "=======~n"
                                       "Result: ~p~n"
                                       "History: ~p~n",
                                       [Cmds,
                                        State,Result,History]),
                             aggregate(command_names(Cmds), Result =:= ok))
               end
              )
           ).

-spec run_counterexample() -> any().
run_counterexample() ->
    run_counterexample(proper:counterexample()).

-spec run_counterexample(any()) -> any().
run_counterexample('undefined') -> 'undefined';
run_counterexample([SeqSteps]) ->
    run_counterexample(SeqSteps, initial_state()).

run_counterexample(SeqSteps, State) ->
    process_flag('trap_exit', 'true'),

    _ = kz_datamgr:db_create(?DB),
    _ = kz_datamgr:del_doc(?DB, ?ID),
    _ = kz_datamgr:flush_cache_docs(?DB),

    try lists:foldl(fun transition_if/2
                   ,{1, State, #{}}
                   ,SeqSteps
                   )
    catch
        'throw':T -> {'throw', T}
    after
        kz_datamgr:del_doc(?DB, ?ID),
        kz_datamgr:flush_cache_docs(?DB)
    end.


transition_if({'set', Var, Call}, {Step, State, Env}) ->
    {'call', M, F, As} = Call,
    Resp = erlang:apply(M, F, fix_args(As, Env)),
    io:format('user', "~w: at state ~s: ~w -> ~s~n", [Step, State, Call, Resp]),

    case postcondition(State, Call, Resp) of
        'true' ->
            {Step+1, next_state(State, Resp, Call), Env#{Var =>Resp}};
        'false' ->
            io:format("failed on step ~p~n", [Step]),
            throw({'failed_postcondition', State, Call, Resp})
    end.

fix_args(Args, Env) ->
    lists:foldr(fun(A, Acc) -> fix_arg(A, Acc, Env) end, [], Args).

fix_arg(Arg, Acc, Env) ->
    case maps:get(Arg, Env, 'undefined') of
        'undefined' -> [Arg | Acc];
        EnvArg -> [EnvArg | Acc]
    end.

-spec create() -> kz_term:ne_binary().
create() ->
    {'ok', D} = kz_datamgr:save_doc(?DB, kz_json:from_list(doc())),
    kz_doc:revision(D).

-spec update(kz_term:ne_binary()) -> kz_term:ne_binary().
update(Rev) ->
    {'ok', D} = kz_datamgr:update_doc(?DB, ?ID, [{'update', doc(Rev)}, {'ensure_saved', 'true'}]),
    kz_doc:revision(D).

-spec get(kz_term:api_ne_binary()) -> kz_term:api_ne_binary().
get(_Rev) ->
    case kz_datamgr:open_cache_doc(?DB, ?ID) of
        {'ok', D} -> kz_doc:revision(D);
        {'error', 'not_found'} -> 'undefined'
    end.

doc() ->
    doc('undefined').

doc(Rev) ->
    [{<<"_id">>, ?ID}
    ,{<<"_rev">>, Rev}
    ,{<<"foo">>, <<"bar">>}
    ,{<<"pvt_account_db">>, ?DB}
     | [{kz_term:to_binary(Key), kz_binary:rand_hex(16)} || Key <- lists:seq(1,50)]
    ].

-spec initial_state() -> 'undefined'.
initial_state() ->
    'undefined'.

-spec command(kz_term:api_ne_biary()) -> tuple().
command('undefined') ->
    oneof([{'call', ?MODULE, 'create', []}
          ,{'call', ?MODULE, 'get', ['undefined']}
          ]);
command(Rev) ->
    oneof([{'call', ?MODULE, 'update', [Rev]}
          ,{'call', ?MODULE, 'get', [Rev]}
           %% ,{'call', 'timer', 'sleep', [range(0,50)]}
          ]).

-spec next_state(kz_term:api_ne_binary(), any(), tuple()) -> kz_term:api_ne_binary().
next_state(_OldRev
          ,NewRev
          ,{'call', ?MODULE, 'create', []}
          ) ->
    NewRev;
next_state(_OldRev
          ,NewRev
          ,{'call', ?MODULE, 'update', [_]}
          ) ->
    NewRev;
next_state(Rev, _V, {'call', ?MODULE, 'get', [_]}) ->
    Rev;
next_state(Rev, _V, {'call', 'timer', 'sleep', [_]}) ->
    Rev.

-spec precondition(any(), any()) -> 'true'.
precondition(_Method, _Call) -> 'true'.

-spec postcondition(kz_term:api_ne_binary(), tuple(), kz_term:ne_binary()) -> boolean().
postcondition(NoRev
             ,{'call', ?MODULE, 'create', []}
             ,Rev
             ) ->
    NoRev =:= 'undefined'
        andalso is_binary(Rev);
postcondition(OldRev
             ,{'call', ?MODULE, 'update', [_OldRev]}
             ,NewRev
             ) ->
    [OldN, _OldRand] = binary:split(OldRev, <<"-">>),
    [NewN, _NewRand] = binary:split(NewRev, <<"-">>),

    kz_term:to_integer(NewN) =:= kz_term:to_integer(OldN) + 1;
postcondition(OldRev
             ,{'call', ?MODULE, 'get', [_OldRev]}
             ,GetRev
             ) ->
    OldRev =:= GetRev;
postcondition(_Rev, {'call', 'timer', 'sleep', [_]}, _Res) -> 'true'.
