-module(hoolva_chat).

-behaviour(gen_server).

% -include("emqx_retainer.hrl").
% -include_lib("emqx/include/emqx.hrl").
% -include_lib("emqx/include/logger.hrl").
-include("emqx.hrl").
% -include("logger.hrl").             %change to above code when connected to server
% -include_lib("stdlib/include/ms_transform.hrl").
% -include_lib("stdlib-3.17.2.1/include/ms_transform.hrl").


% -logger_header("[hoolva_chat]").

% -export([start_link/1]).

-export([ load/1
        , unload/0
        ]).

-export([ 
    % on_session_subscribed/4
         on_message_delivered/3
        , on_message_publish/2
        , on_message_acked/3
        ]).

% -export([clean/1]).

% %% gen_server callbacks
% -export([ init/1
%         , handle_call/3
%         , handle_cast/2
%         , handle_info/2
%         , terminate/2
%         , code_change/3
%         ]).


load(Env) ->
    % emqx:hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
    emqx:hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
    emqx:hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
    emqx:hook('message.acked',       {?MODULE, on_message_acked, [Env]}).

unload() -> 
    % emqx:unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
    emqx:unhook('message.publish',     {?MODULE, on_message_publish}),
    emqx:unhook('message.delivered',   {?MODULE, on_message_delivered}),
    emqx:unhook('message.acked',       {?MODULE, on_message_acked}).



% on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
%     io:format("~n ------Session(~s) subscribed ~p with subopts: ~p~n", [ClientId, Topic, SubOpts]),
%     % B = hoolva_chat_actions:get_chat(#{topic => Topic, status => <<"undelivered">> }),
%     case hoolva_chat_actions:get_chat(#{topic => Topic, status => <<"undelivered">> }) of
%     % case B of
%         [] ->
%             io:format("~nno undelivered messsage found");
%         C ->
%             List_length = length(C),
%             io:format("~n ~p undelivered message found",[List_length]),
%             hoolva_chat_actions:retained(Topic,C)
%         % [] ->
%         %     io:format("~n~nno data found")
%             % ok
%         end.


% on_session_subscribed(_, _, #{share := ShareName}, _Env) when ShareName =/= undefined ->
%     ok;
% on_session_subscribed(_, Topic, #{rh := Rh, is_new := IsNew}, _Env) ->
%     case Rh =:= 0 orelse (Rh =:= 1 andalso IsNew) of
%         true -> emqx_pool:async_submit(fun dispatch/2, [self(), Topic]);
%         _ -> ok
%     end.


% on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
%     io:format("Message delivered to client(~s): ~s~n",
%               [ClientId, emqx_message:format(Message)]),
%     {ok, Message}.

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message delivered to client : ~p~n message : ~p~n",[ClientId, Message]),
    {ok, Message}.

on_message_acked(#{client_id := ClientId}, Message, _Env) ->
    % io:format("Session(~s) acked message: ~s~n", [ClientId, emqx_message:format(Message)]),
    io:format("~n----------client_id : ~p ~nmessage : ~p~n",[ClientId,Message]),
    % voifinity_message_action:on_delivered(ClientId,Message),
    {ok, Message}.

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

% on_message_publish(Message = #message{headers = #{}}, _Env) ->
%     P = element(5, Message),
%     io:format("~n ----- task ------- ~n~p ~n P = ~p~n",[Message,P]),
%     {ok,Message};

% on_message_publish(Message, _Env) ->
%     % io:format("Publish ~s~n", [emqx_message:format(Message)]),
%     io:format("-------------home ---~nPublish = ~p~n", [Message]),
%     hoolva_chat_actions:store(Message),
%     {ok, Message}.

on_message_publish(Message, _Env) ->
    % P = element(5, Message),
    % case P == #{} of 
    %     true  -> 
    %         % io:format("~n ----- task ------- ~n~p ~n P = ~p~n",[Message,P]),
    %         {ok,Message};
    %     false ->
    %         io:format("~nrecieved on published message"),
            io:format("-------------home ---~nPublish = ~p~n", [Message]),
    %         {_,Uuid} = hoolva_chat_actions:store(Message),
    %         io:format("~n ----- ~p -------~n",[Uuid]),
    %         % hoolva_chat_actions:send(Uuid),
            {ok, Message}.
        % end.
    
% %----- on session subscribed -- subs ---

% topic2tokens(Topic) ->
%     emqx_topic:words(Topic).

% dispatch(Pid, Topic) ->
%     Msgs = case emqx_topic:wildcard(Topic) of
%                false -> read_messages(Topic);
%                true  -> match_messages(Topic)
%            end,
%     [Pid ! {deliver, Topic, Msg} || Msg  <- sort_retained(Msgs)].


% -spec(read_messages(emqx_types:topic())
%       -> [emqx_types:message()]).
% read_messages(Topic) ->
%     Tokens = topic2tokens(Topic),
%     case mnesia:dirty_read(?TAB, Tokens) of
%         [] -> [];
%         [#retained{msg = Msg, expiry_time = Et}] ->
%             case Et =:= 0 orelse Et >= erlang:system_time(millisecond) of
%                 true -> [Msg];
%                 false -> []
%             end
%     end.

% -spec(match_messages(emqx_types:topic())
%       -> [emqx_types:message()]).
% match_messages(Filter) ->
%     NowMs = erlang:system_time(millisecond),
%     Cond = condition(emqx_topic:words(Filter)),
%     MsHd = #retained{topic = Cond, msg = '$2', expiry_time = '$3'},
%     Ms = [{MsHd, [{'=:=','$3',0}], ['$2']},
%           {MsHd, [{'>','$3',NowMs}], ['$2']}],
%     mnesia:dirty_select(?TAB, Ms).

% sort_retained([]) -> [];
% sort_retained([Msg]) -> [Msg];
% sort_retained(Msgs)  ->
%     lists:sort(fun(#message{timestamp = Ts1}, #message{timestamp = Ts2}) ->
%                    Ts1 =< Ts2
%                end, Msgs).