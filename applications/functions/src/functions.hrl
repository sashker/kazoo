-ifndef(FUNCTIONS_HRL).
-include_lib("kazoo_stdlib/include/kz_types.hrl").
-include_lib("kazoo_stdlib/include/kz_log.hrl").
-include_lib("kazoo_stdlib/include/kz_databases.hrl").
-include_lib("kazoo_events/include/kz_hooks.hrl").

-define(APP, functions).
-define(APP_NAME, (atom_to_binary(?APP, utf8))).
-define(APP_VERSION, <<"4.0.0">>).

-define(TABLE, 'functions_listener').

-type http_verb() :: 'get' | 'post'.
-type trigger_retries() :: 1..5.

%
% use function_record since function is a reserved term.
-record(function_record, {id :: kz_term:api_ne_binary() | '_'
                 ,uri :: kz_term:api_ne_binary() | '_'
                 ,http_verb = 'get' :: http_verb() | '_'
                 ,trigger_event :: kz_term:api_ne_binary() | '_' | '$1' | '$2'
                 ,trigger_id :: kz_term:api_ne_binary() | '_'
                 ,retries = 3 :: trigger_retries() | '_'
                 ,account_id :: kz_term:api_ne_binary() | '_' | '$1'
                 ,include_subaccounts = 'false' :: boolean() | '_' | '$3'
                 ,include_loopback = 'true' :: boolean() | '_'
                 ,custom_data :: kz_term:api_object() | '_'
                 ,modifiers :: kz_term:api_object() | '_'
                 ,format = 'form-data' :: 'form-data' | 'json' | '_'
                 }).

-type function_record() :: #function_record{}.
-type function_records() :: [function_record()].

-define(CACHE_NAME, 'functions_cache').

-define(ATTEMPT_EXPIRY_KEY, <<"attempt_failure_expiry_ms">>).
-define(FAILURE_COUNT_KEY, <<"attempt_failure_count">>).

-define(FAILURE_CACHE_KEY(AccountId, HookId, Timestamp)
       ,{'failure', AccountId, HookId, Timestamp}
       ).

-define(FUNCTION_META_LIST, <<"functions/function_meta_listing">>).

-define(FUNCTIONS_HRL, 'true').
-endif.
