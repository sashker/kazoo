%%%-------------------------------------------------------------------
%%% @copyright (C) 2011-2013, VoIP INC
%%% @doc
%%%
%%% @end
%%% @contributions
%%%
%%%-------------------------------------------------------------------
-module(wh_amqp_channel).

-export([consumer_pid/0
         ,consumer_pid/1
        ]).
-export([new/0
         ,new/1
        ]).
-export([open/2]).
-export([close/0
         ,close/1
        ]).
-export([remove/0
         ,remove/1
        ]).
-export([publish/2]).
-export([consume/1]).

-include("amqp_util.hrl").

consumer_pid() ->
    case get('$wh_amqp_consumer') of
        Pid when is_pid(Pid) -> Pid;
        _Else -> self()
    end.

consumer_pid(Pid) ->
    put('$wh_amqp_consumer', Pid).

-spec new/1 :: (#wh_amqp_channel{}) -> #wh_amqp_channel{} | {'error', _}.
new() ->
    Pid = consumer_pid(),
    new(wh_amqp_channels:add(Pid)).

new(#wh_amqp_channel{}=Channel) ->
    case wh_amqp_connections:current() of
        {error, _}=E -> E;
        {ok, Connection} ->
            open(Channel, Connection)
    end.

close() ->
    case wh_amqp_channels:find() of
        {error, _} -> ok;
        #wh_amqp_channel{}=Channel ->
            close(Channel)
    end.

close(#wh_amqp_channel{channel=Pid, commands=[#'basic.consume'{consumer_tag=CTag}
                                              |Commands]}=Channel) when is_pid(Pid) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    catch amqp_channel:call(Pid, #'basic.cancel'{consumer_tag=CTag}),
    close(Channel#wh_amqp_channel{commands=Commands});
close(#wh_amqp_channel{commands=[_|Commands]}=Channel) ->
    close(Channel#wh_amqp_channel{commands=Commands});
close(#wh_amqp_channel{channel=Pid}=Channel) when is_pid(Pid) ->
    lager:debug("closed channel ~p", [Pid]),
    C = wh_amqp_channels:demonitor_channel(Channel),
    catch amqp_channel:close(Pid),
    C;
close(Channel) ->
    Channel.

remove() ->
    case wh_amqp_channels:find() of
        {error, _} -> ok;
        #wh_amqp_channel{}=Channel ->
            remove(Channel)
    end.

remove(#wh_amqp_channel{consumer_ref=Ref}=Channel) when is_reference(Ref) ->
    remove(wh_amqp_channels:demonitor_consumer(Channel));
remove(#wh_amqp_channel{channel=Pid}=Channel) when is_pid(Pid) ->
    remove(close(Channel));
remove(#wh_amqp_channel{}=Channel) ->
    wh_amqp_channels:remove(Channel).

-spec publish/2 :: (#'basic.publish'{}, ne_binary() | iolist()) -> 'ok' | {'error', _}.
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub
        ,#'amqp_msg'{props=#'P_basic'{timestamp=undefined}=Props}=AmqpMsg) ->
    Now = wh_util:now_us(now()),
    publish(BasicPub, AmqpMsg#'amqp_msg'{props=Props#'P_basic'{timestamp=Now}});
publish(#'basic.publish'{exchange=_Exchange, routing_key=_RK}=BasicPub, AmqpMsg) ->
    case wh_amqp_channels:get_channel() of
        {error, _R} ->
            lager:warning("unable to delever payload to ~s exchange (routing key ~s): ~p", [_Exchange, _RK, _R]),
            ok;
        #wh_amqp_channel{channel=Pid} when is_pid(Pid) ->
            amqp_channel:call(Pid, BasicPub, AmqpMsg),
%%            lager:debug("published to ~s exchange (routing key ~s) via ~p: ~s", [_Exchange, _RK, Pid, AmqpMsg#'amqp_msg'.payload]),
            lager:debug("published to ~s exchange (routing key ~s) via ~p", [_Exchange, _RK, Pid]),
            ok;
        #wh_amqp_channel{} ->
            lager:debug("dropping payload to ~s exchange (routing key ~s): ~s", [_Exchange, _RK, AmqpMsg#'amqp_msg'.payload]),
            ok
    end.

-spec consume/1 :: (consume_records()) -> consume_ret().
consume(Command) ->
    case wh_amqp_channels:get_channel() of
        {error, _}=E -> E;
        #wh_amqp_channel{}=Channel ->
            consume(Channel, Command)
    end.

consume(#wh_amqp_channel{channel=Pid}, #'basic.ack'{}=BasicAck) ->
    amqp_channel:cast(Pid, BasicAck);
consume(#wh_amqp_channel{channel=Pid}, #'basic.nack'{}=BasicNack) ->
    amqp_channel:cast(Pid, BasicNack);
consume(#wh_amqp_channel{consumer=Consumer, channel=Pid, commands=Commands}=Channel
        ,#'basic.consume'{}=Command) ->
    case lists:member(#'basic.consume'{}, Commands) of
        true -> ok;
        false ->
            Result = amqp_channel:subscribe(Pid, Command, Consumer),
            handle_command_result(Result, Command, Channel)
    end;
consume(#wh_amqp_channel{channel=Pid, commands=Commands}=Channel
        ,#'basic.cancel'{nowait=NoWait}) ->
    lists:foreach(fun(#'basic.consume'{consumer_tag=CTag}) ->
                          Command = #'basic.cancel'{consumer_tag=CTag, nowait=NoWait},
                          Result = amqp_channel:call(Pid, Command),
                          handle_command_result(Result, Command, Channel);
                     (_) ->
                          ok
                  end, Commands);
consume(#wh_amqp_channel{}=Channel
        ,#'exchange.declare'{}=Command) ->
    maybe_declare_exchange(Command, Channel);
consume(#wh_amqp_channel{channel=Pid}=Channel, Command) ->
    Result = amqp_channel:call(Pid, Command),
    handle_command_result(Result, Command, Channel).

maybe_declare_exchange(#'exchange.declare'{}=Command
                       ,#wh_amqp_channel{channel=Pid}=Channel) ->
    case wh_amqp_connection:does_exchange_exist(Channel, Command) of
        true -> ok;
        false ->
            Result = amqp_channel:call(Pid, Command),
            handle_command_result(Result, Command, Channel)
    end.

handle_command_result({error, _}=Error, _, _) ->
    Error;
handle_command_result({ok, Ok}, Command, Channel) ->
    handle_command_result(Ok, Command, Channel);
handle_command_result(#'exchange.declare_ok'{}
                      ,#'exchange.declare'{exchange=_Ex, type=_Ty}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("declared ~s exchange ~s via channel ~p", [_Ty, _Ex, Pid]),
    wh_amqp_connection:exchange_exist(Channel, Command),
    ok;
handle_command_result(#'basic.qos_ok'{}
                      ,#'basic.qos'{prefetch_count=Prefetch}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("applied QOS prefetch ~p to channel ~p", [Prefetch, Pid]),
    wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'queue.delete_ok'{}
                      ,#'queue.delete'{queue=Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("deleted queue ~s via channel ~p", [Q, Pid]),
    wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'queue.declare_ok'{queue=Q}=Ok
                      ,#'queue.declare'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("declared queue ~s via channel ~p", [Q, Pid]),
    wh_amqp_channels:command(Channel, Command#'queue.declare'{queue=Q}),
    {ok, Ok};
handle_command_result(#'queue.unbind_ok'{}
                      ,#'queue.unbind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("unbound ~s from ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'queue.bind_ok'{}
                      ,#'queue.bind'{exchange=_Exchange, routing_key=_RK, queue=_Q}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("bound ~s to ~s exchange (routing key ~s) via channel ~p", [_Q, _Exchange, _RK, Pid]),
    wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(#'basic.consume_ok'{consumer_tag=CTag}
                      ,#'basic.consume'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("created consumer ~s via channel ~p", [CTag, Pid]),
    wh_amqp_channels:command(Channel, Command#'basic.consume'{consumer_tag=CTag}),
    ok;
handle_command_result(#'basic.cancel_ok'{consumer_tag=CTag}
                      ,#'basic.cancel'{}=Command
                      ,#wh_amqp_channel{channel=Pid}=Channel) ->
    lager:debug("canceled consumer ~s via channel ~p", [CTag, Pid]),
    wh_amqp_channels:command(Channel, Command),
    ok;
handle_command_result(_Else, _, _) ->
    lager:warning("unexpected AMQP command result: ~p", [_Else]),
    {error, unexpected_result}.

-spec open/2 :: (#wh_amqp_connection{}, #wh_amqp_channel{} | pid()) -> #wh_amqp_channel{}.
open(Channel, #wh_amqp_connection{connection=ConnectionPid}=Connection) ->
    case wh_amqp_connection:get_channel(Connection) of
        {ok, Pid} ->
            %%            amqp_selective_consumer:register_default_consumer(Channel, Srv),
            Routines = [fun(C) ->
                                C#wh_amqp_channel{channel=Pid
                                                  ,connection=ConnectionPid}
                        end
                        ,fun(C) -> wh_amqp_channels:monitor_channel(C) end
                        ,fun(C) -> wh_amqp_channels:monitor_consumer(C) end
                        ,fun(#wh_amqp_channel{commands=Commands}=C) ->
                                 maybe_reestablish(C#wh_amqp_channel{commands=lists:reverse(Commands)
                                                                     ,reconnecting=true}) 
                         end
                       ],
            io:format("create channel ~p for ~p~n", [Pid, Channel#wh_amqp_channel.consumer]),
            lists:foldl(fun(F, C) -> F(C) end, Channel, Routines);
        {error, _R} ->
            close(Channel)
    end;
open(Channel, _) ->
    lager:critical("failed to open AMQP channel: no_connection", []),
    close(Channel).

maybe_reestablish(#wh_amqp_channel{commands=[]}=Channel) ->
    Channel#wh_amqp_channel{reconnecting=false};
maybe_reestablish(#wh_amqp_channel{commands=[Command|Commands]}=Channel) ->
    _ = consume(Channel#wh_amqp_channel{commands=[]}, Command),
    maybe_reestablish(Channel#wh_amqp_channel{commands=Commands}).
