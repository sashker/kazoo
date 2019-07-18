%%%-----------------------------------------------------------------------------
%%% @copyright (C) 2013-2019, 2600Hz
%%% @doc
%%% @author Ming Luo
%%% @end
%%%-----------------------------------------------------------------------------
-module(functions_listener).

-behaviour(gen_listener).

-export([start_link/0
        ,handle_config/2
        ]).
-export([init/1
        ,handle_call/3
        ,handle_cast/2
        ,handle_info/2
        ,handle_event/2
        ,terminate/2
        ,code_change/3
        ]).

-export([func_id/1]).

-include("functions.hrl").
-include_lib("kazoo_amqp/include/kapi_conf.hrl").

-define(SERVER, ?MODULE).

-record(state, {}).
-type state() :: #state{}.

-define(BINDINGS, [{'conf', [{'action', <<"*">>}
                            ,{'db', ?KZ_FUNCTIONS_DB}
                            ,{'type', <<"function">>}
                            ,{'id', <<"*">>}
                            ,'federate'
                            ]}
                  ]).

-define(RESPONDERS, [{{?MODULE, 'handle_config'}
                     ,[{<<"configuration">>, <<"*">>}]
                     }
                    ]).

%%%=============================================================================
%%% API
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Starts the server.
%% @end
%%------------------------------------------------------------------------------
-spec start_link() -> kz_types:startlink_ret().
start_link() ->
    gen_listener:start_link(?SERVER
                           ,[{'bindings', ?BINDINGS}
                            ,{'responders', ?RESPONDERS}
                            ]
                           ,[]
                           ).

-spec handle_config(kz_json:object(), kz_term:proplist()) -> 'ok'.
handle_config(JObj, Props) ->
    'true' = kapi_conf:doc_update_v(JObj),
    handle_config(JObj
                 ,props:get_value('server', Props)
                 ,kz_api:event_name(JObj)
                 ).

-spec handle_config(kz_json:object(), pid(), kz_term:ne_binary()) -> 'ok'.
handle_config(_JObj, _Srv, ?DOC_CREATED) ->
    lager:debug("doc_created"),
    ok;
handle_config(_JObj, _Srv, ?DOC_EDITED) ->
    lager:debug("doc_edited"),
    ok;
handle_config(_JObj, _Srv, ?DOC_DELETED) ->
    lager:debug("doc_deleted"),
    ok.

%%%=============================================================================
%%% gen_listener callbacks
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc Initializes the server.
%% @end
%%------------------------------------------------------------------------------
-spec init([]) -> {'ok', state()}.
init([]) ->
    _ = kz_util:put_callid(?MODULE),
    {'ok', #state{}}.

%%------------------------------------------------------------------------------
%% @doc Handling call messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_call(any(), kz_term:pid_ref(), state()) -> kz_types:handle_call_ret_state(state()).
handle_call(_Request, _From, State) ->
    {'reply', {'error', 'not_implemented'}, State}.

%%------------------------------------------------------------------------------
%% @doc Handling cast messages.
%% @end
%%------------------------------------------------------------------------------
%% TODO: add impl for amqp listener handler
-spec handle_cast(any(), state()) -> kz_types:handle_cast_ret_state(state()).
handle_cast(_Msg, State) ->
    lager:debug("unhandled cast: ~p", [_Msg]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Handling all non call/cast messages.
%% @end
%%------------------------------------------------------------------------------
-spec handle_info(any(), state()) -> kz_types:handle_info_ret_state(state()).
handle_info({'ETS-TRANSFER', _TblId, _From, _Data}, State) ->
    lager:debug("write access to table '~p' available", [_TblId]),
    Self = self(),
    _ = kz_util:spawn(
          fun() ->
                  kz_util:put_callid(?MODULE)
                      , load_funcs(Self)
          end),
    {'noreply', State};
handle_info(_Info, State) ->
    lager:debug("unhandled msg: ~p", [_Info]),
    {'noreply', State}.

%%------------------------------------------------------------------------------
%% @doc Allows listener to pass options to handlers.
%% @end
%%------------------------------------------------------------------------------
-spec handle_event(kz_json:object(), kz_term:proplist()) -> gen_listener:handle_event_return().
handle_event(_JObj, _State) ->
    {'reply', []}.

%%------------------------------------------------------------------------------
%% @doc This function is called by a `gen_server' when it is about to
%% terminate. It should be the opposite of `Module:init/1' and do any
%% necessary cleaning up. When it returns, the `gen_server' terminates
%% with Reason. The return value is ignored.
%%
%% @end
%%------------------------------------------------------------------------------
-spec terminate(any(), state()) -> 'ok'.
terminate(_Reason, _State) ->
    lager:debug("listener terminating: ~p", [_Reason]).

%%------------------------------------------------------------------------------
%% @doc Convert process state when code is changed.
%% @end
%%------------------------------------------------------------------------------
-spec code_change(any(), state(), any()) -> {'ok', state()}.
code_change(_OldVsn, State, _Extra) ->
    {'ok', State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%%------------------------------------------------------------------------------
%% @doc
%% @end
%%------------------------------------------------------------------------------
-spec load_funcs(pid()) -> 'ok'.
load_funcs(_Srv) ->
    lager:debug("loading functions into memory"),
    ok.

-spec func_id(kz_json:object()) -> kz_term:ne_binary().
func_id(JObj) ->
    func_id(kz_json:get_first_defined([<<"pvt_account_id">>
                                      ,<<"Account-ID">>
                                      ]
                                     ,JObj
                                     )
           ,kz_json:get_first_defined([<<"_id">>, <<"ID">>], JObj)
           ).

-spec func_id(kz_term:ne_binary(), kz_term:ne_binary()) -> kz_term:ne_binary().
func_id(AccountId, Id) ->
    <<AccountId/binary, ".", Id/binary>>.
