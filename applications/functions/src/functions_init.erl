%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2010-2019, 2600Hz
%%% @doc
%%% @end
%%%-----------------------------------------------------------------------------
-module(functions_init).

-export([start_link/0
        ,init_modules/0
        ,existing_modules/0
        ]).

-include("functions.hrl").

-spec start_link() -> 'ignore'.
start_link() ->
    kz_util:put_callid(?MODULE),
    _ = kz_util:spawn(fun do_init/0),
    'ignore'.

-spec do_init() -> 'ok'.
do_init() ->
    init_dbs(),
    init_modules().

-spec init_dbs() -> 'ok'.
init_dbs() ->
    _ = kz_datamgr:db_create(?KZ_FUNCTIONS_DB),
    _ = kz_datamgr:revise_doc_from_file(?KZ_SCHEMA_DB, 'crossbar', <<"schemas/functions.json">>),
    'ok'.

-spec init_modules() -> 'ok'.
init_modules() ->
    lists:foreach(fun init_module/1, existing_modules()).

-spec init_module(atom()) -> 'ok'.
init_module(Module) ->
    %% TODO: to be confirmed whether each module should be initiated or started.
    lager:debug("initializing ~s", [Module]).

-spec existing_modules() -> kz_term:atoms().
existing_modules() ->
    {ok, Modules} = application:get_key('functions', 'modules'),
    Modules.
