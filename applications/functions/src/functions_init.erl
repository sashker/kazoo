%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(functions_init).

-export([start_link/0
        ,init_modules/0
        ,existing_modules/0
        ,maybe_init_account/2
        ]).

-include("functions.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    lager:warning("functions init: start_link()"),
    kz_util:put_callid(?MODULE),
    _ = kz_util:spawn(fun do_init/0),
    'ignore'.

-spec do_init() -> 'ok'.
do_init() ->
    init_dbs(),
    init_modules().

-spec do_init(kz_term:ne_binary()) -> 'ok'.
do_init(MasterAccountDb) ->
    init_master_account_db(MasterAccountDb),
    init_modules().

-spec init_dbs() -> 'ok'.
init_dbs() ->
    lager:warning("functions init_db()"),
    _ = init_master_account_db(),
    _ = kz_datamgr:db_create(?KZ_FUNCTIONS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, 'crossbar', <<"schemas/functions.json">>),
    'ok'.

-spec maybe_init_account(kz_json:object(), kz_term:proplist()) -> 'ok' | 'false'.
maybe_init_account(JObj, _Props) ->
    Database = kapi_conf:get_database(JObj),
    kz_datamgr:db_classification(Database) =:= 'account'
        andalso do_init(Database).

-spec init_master_account_db() -> 'ok'.
init_master_account_db() ->
    case kapps_util:get_master_account_db() of
        {'ok', MasterAccountDb} ->
            init_master_account_db(MasterAccountDb);
        {'error', _} ->
            lager:warning("master account hasn't been created yet")
    end.

-spec init_master_account_db(kz_term:ne_binary()) -> 'ok'.
init_master_account_db(MasterAccountDb) ->
    _ = kapps_maintenance:refresh(MasterAccountDb),
    lager:debug("loaded view into master db ~s", [MasterAccountDb]).

-spec init_modules() -> 'ok'.
init_modules() ->
    lists:foreach(fun init_module/1, existing_modules()),
    lager:debug("finished initializing modules").

-spec init_module(atom()) -> 'ok'.
init_module(Module) ->
    lager:debug("initializing ~s", [Module]),
    try Module:init() of
        _ -> lager:debug("~s initialized", [Module])
    catch
        'error':'undef' ->
            lager:debug("~s doesn't export init/0", [Module]);
        _E:_R ->
            lager:debug("~s failed: ~s: ~p", [Module, _E, _R])
    end.

-spec existing_modules() -> kz_term:atoms().
existing_modules() ->
    existing_modules(code:lib_dir(kz_term:to_atom(?APP_NAME))).

-spec existing_modules(string()) -> kz_term:atoms().
existing_modules(FunctionsRoot) ->
    ModulesDirectory = filename:join(FunctionsRoot, "ebin"),
    Extension = ".beam",
    Utils = ["functions_app"
            ,"functions_init"
            ,"functions_listener"
            ,"functions_maintenance"
            ,"functions_sup"
            ],
    Pattern = filename:join(ModulesDirectory, "*"++Extension),
    [kz_term:to_atom(Module, 'true')
     || Path <- filelib:wildcard(Pattern),
        not lists:member((Module=filename:basename(Path, Extension)), Utils)
    ].
