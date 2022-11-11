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

% -export([ 
%     % on_session_subscribed/4
%          on_message_delivered/3
%         , on_message_publish/2
%         , on_message_acked/3
%         ]).

% -export([clean/1]).

% %% gen_server callbacks
% -export([ init/1
%         , handle_call/3
%         , handle_cast/2
%         , handle_info/2
%         , terminate/2
%         , code_change/3
%         ]).
%% Client Lifecircle Hooks
-export([on_client_connect/3
  , on_client_connack/4
  , on_client_connected/3
  , on_client_disconnected/4
  , on_client_authenticate/3
  , on_client_check_acl/5
  , on_client_subscribe/4
  , on_client_unsubscribe/4
]).

%% Session Lifecircle Hooks
-export([on_session_created/3
  , on_session_subscribed/4
  , on_session_unsubscribed/4
  , on_session_resumed/3
  , on_session_discarded/3
  , on_session_takeovered/3
  , on_session_terminated/4
]).

%% Message Pubsub Hooks
-export([on_message_publish/2
  , on_message_delivered/3
  , on_message_acked/3
  , on_message_dropped/4
]).


load(Env) ->
  emqx:hook('client.connect', {?MODULE, on_client_connect, [Env]}),
  emqx:hook('client.connack', {?MODULE, on_client_connack, [Env]}),
  emqx:hook('client.connected', {?MODULE, on_client_connected, [Env]}),
  emqx:hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
  emqx:hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
  emqx:hook('client.check_acl', {?MODULE, on_client_check_acl, [Env]}),
  emqx:hook('client.subscribe', {?MODULE, on_client_subscribe, [Env]}),
  emqx:hook('client.unsubscribe', {?MODULE, on_client_unsubscribe, [Env]}),
  emqx:hook('session.created', {?MODULE, on_session_created, [Env]}),
  emqx:hook('session.subscribed', {?MODULE, on_session_subscribed, [Env]}),
  emqx:hook('session.unsubscribed', {?MODULE, on_session_unsubscribed, [Env]}),
  emqx:hook('session.resumed', {?MODULE, on_session_resumed, [Env]}),
  emqx:hook('session.discarded', {?MODULE, on_session_discarded, [Env]}),
  emqx:hook('session.takeovered', {?MODULE, on_session_takeovered, [Env]}),
  emqx:hook('session.terminated', {?MODULE, on_session_terminated, [Env]}),
  emqx:hook('message.publish', {?MODULE, on_message_publish, [Env]}),
  emqx:hook('message.delivered', {?MODULE, on_message_delivered, [Env]}),
  emqx:hook('message.acked', {?MODULE, on_message_acked, [Env]}),
  emqx:hook('message.dropped', {?MODULE, on_message_dropped, [Env]}).

unload() -> 
  emqx:unhook('client.connect', {?MODULE, on_client_connect}),
  emqx:unhook('client.connack', {?MODULE, on_client_connack}),
  emqx:unhook('client.connected', {?MODULE, on_client_connected}),
  emqx:unhook('client.disconnected', {?MODULE, on_client_disconnected}),
  emqx:unhook('client.authenticate', {?MODULE, on_client_authenticate}),
  emqx:unhook('client.check_acl', {?MODULE, on_client_check_acl}),
  emqx:unhook('client.subscribe', {?MODULE, on_client_subscribe}),
  emqx:unhook('client.unsubscribe', {?MODULE, on_client_unsubscribe}),
  emqx:unhook('session.created', {?MODULE, on_session_created}),
  emqx:unhook('session.subscribed', {?MODULE, on_session_subscribed}),
  emqx:unhook('session.unsubscribed', {?MODULE, on_session_unsubscribed}),
  emqx:unhook('session.resumed', {?MODULE, on_session_resumed}),
  emqx:unhook('session.discarded', {?MODULE, on_session_discarded}),
  emqx:unhook('session.takeovered', {?MODULE, on_session_takeovered}),
  emqx:unhook('session.terminated', {?MODULE, on_session_terminated}),
  emqx:unhook('message.publish', {?MODULE, on_message_publish}),
  emqx:unhook('message.delivered', {?MODULE, on_message_delivered}),
  emqx:unhook('message.acked', {?MODULE, on_message_acked}),
  emqx:unhook('message.dropped', {?MODULE, on_message_dropped}).


on_client_connect(ConnInfo = #{clientid := ClientId}, Props, _Env) ->
    % io:format("Client(~s) connect, ConnInfo: ~p, Props: ~p~n",[ClientId, ConnInfo, Props]),
    {ok, Props}.

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
    % io:format("Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",[ClientId, ConnInfo, Rc, Props]),
    {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->                    %% This API is called after a mqtt client has establish a connection with broker.
    io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ClientInfo, ConnInfo]).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->         %% This API is called after a mqtt client has disconnected
  io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]).

on_client_authenticate(ClientInfo = #{clientid := ClientId}, Result, Env) ->
%   io:format("Client(~s) authenticate, ClientInfo:~n~p~n, Result:~p,~nEnv:~p~n",[ClientId, ClientInfo, Result, Env]),
  {ok, Result}.

on_client_check_acl(_ClientInfo = #{clientid := ClientId}, Topic, PubSub, Result, _Env) ->
    % io:format("Client(~s) check_acl, PubSub:~p, Topic:~p, Result:~p~n",[ClientId, PubSub, Topic, Result]),
    {ok, Result}.

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->                    %% This API is called before mqtt engine process client's subscribe command. It is possible to change topic or cancel it.
    io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->                  %% This API is called before mqtt engine process client's unsubscribe command. It is possible to change topic or cancel it.
    io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

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


%%--------------------------------------------------------------------
%% Session LifeCircle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->                                      
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->                             %% This API is called after a subscription has been done.
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->                              %% This API is called after a unsubscription has been done.
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takeovered(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takeovered. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n",
              [ClientId, Reason, SessInfo]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->                       %%This API is called before publishing message into mqtt engine. It's possible to change message or cancel publish in this API.
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
    io:format("-------------home ---~nPublish = ~p~n", [Message]),
    % {FromClientId, FromUsername} = parse_form(Message),
    hoolva_chat_actions:store(Message),
    {ok, Message}.

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->                           %% This API is called after a message has been pushed to mqtt clients
    io:format("Message delivered to client : ~p~n message : ~p~n",[ClientId, Message]),                
    {ok, Message}.


on_message_acked(_ClientInfo, #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    io:format("~n------------ checking ACK ---------~n"),
    ok;
on_message_acked(#{clientid := ClientId}, Message, _Env) ->                                              %% delivered and reveived successfully at client  -- This API is called after a message has been acknowledged.
    io:format("~n ======= Message acked by client(~s):~n~p~n", [ClientId, Message]), 
    Message_id = element(2, Message),
    Q = hoolva_chat_actions:put_chat(#{message_id => Message_id, status => <<"delivered">>}),
    io:format("~n UPDATED ~p~n",[Q]).                      

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
    io:format(" DROPED ++++ Message dropped by node ~p due to ~p:~n~p~n",
              [Node, Reason, emqx_message:to_map(Message)]).


% parse_form(Message) ->
%     {emqx_message:from(Message), maybe(emqx_message:get_header(username, Message))}.


% maybe(undefined) -> null;
% maybe(Str) -> Str.


