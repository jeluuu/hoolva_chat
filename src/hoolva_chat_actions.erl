-module(hoolva_chat_actions).

-behaviour(tivan_server).

-export([
    init/1
  % , publish/1
  , store/1
  , group/1
  % , send/1
  , retained/2
  ,put_chat/1
  ,get_chat/0
  ,get_chat/1
  ,delete_chat/1
]).

-export([start_link/0]).

start_link() ->
    tivan_server:start_link({local, ?MODULE}, ?MODULE, [], []).

put_chat(Chat) when is_map(Chat) ->
    tivan_server:put(?MODULE, hoolva_chat, Chat).

get_chat() ->
  get_chat(#{}).

get_chat(Options) when is_map(Options) ->
  tivan_server:get(?MODULE, hoolva_chat, Options).

delete_chat(UserUuid) when is_binary(UserUuid) ->
  delete_chat(#{uuid => UserUuid});
delete_chat(User) when is_map(User) ->
  tivan_server:remove(?MODULE, hoolva_chat, User).

init([]) ->
    TableDefs = #{
        hoolva_chat => #{columns => #{topic => #{type => binary
                                        , limit => 50
                                        , null => false}
                                , message_id => #{type => binary
                                                , key => true}
                                , qos => #{type => integer}
                                , from_id => #{type => binary}
                                , message => #{type => binary}

                                , flags => #{type => map}
                                , headers => #{type => map}

                                , time => #{type => integer}
                                , status => #{type => binary
                                              ,limit => [<<"delivered">>, <<"undelivered">>]
                                              ,default => <<"undelivered">>
                                              ,index => true}
                                        
                                }
                        ,audit => true
                  }
        % topic => #{colums => #{}}
    },
    {ok, TableDefs}.

store(Message) ->
    io:format("~n entered hoolva_chat store function "),       %published by emqx payload
    
    ChatOutput = #{topic => element(7, Message)
                , message_id => element(2, Message)
                , qos => element(3, Message)
                , from_id => element(4, Message)
                , message => hoolva_chat_utils:encrypt(element(8, Message))
                , flags => element(5, Message)
                , headers => element(6, Message)
                , time => element(9, Message)
                
                },
    P = put_chat(ChatOutput),
    io:format("~ndata stored in DB --- ~p~n",[P]),
    {ok,P}.
        
group(Message) ->
  io:format("~nentering group message~n"),
  MsgCheck = element(8,Message),
  case MsgCheck of
    <<"Connection Closed abnormally..!">> ->
        io:format("\nmqtt client closed successfully...!\n");
    _ ->
      DecodedMessage = jsx:decode(element(8,Message)),
      Clientid = element(4, Message),
      From = proplists:get_value(<<"from">>, DecodedMessage),
     
      Topic = proplists:get_value(<<"topic">>,DecodedMessage),
      Message1 = proplists:get_value(<<"message">>,DecodedMessage),
      % Date = proplists:get_value(<<"time">>,DecodedMessage),
      % Time = element(9,Message),
      % Headers = element(6, Message),
      % Flags = element(5, Message),
      Qos = element(3, Message),
      io:format("~n------ Binary === ~p~n",[From]),
      % From0 = list_to_binary(binary_to_list(From)),
      % io:format("~n------ Binary to list === ~p~n",[From0]),
      % From1 = binary_to_list(From),
      % io:format("~n------ Binary to list === ~p~n",[From1]),
      do_group(From,Topic,Qos,Message1,Clientid)
    end.

do_group([],_,_,_,_) ->
  ok;
do_group([H|T],Topic,Qos,Message1,Clientid) ->
  % H1 = list_to_atom(H),
  Topic1 = binary_to_list(H) ++ "/" ++ binary_to_list(Topic),
  Publish = emqx_message:make(Clientid, Qos, Topic1, Message1),
  io:format("~n================ Publish ~p~n",[Publish]),
  emqx:publish(Publish),
  do_group(T,Topic,Qos,Message1,Clientid).




% send(Uuid) ->                                                 %sending to message to topic when already subscribed
%   io:format("~nmessage recived @ send !"),       
%   H = get_chat(Uuid),
%   P = maps:get(message, H),
%   Qos1 = maps:get(qos, H),
%   Topic = maps:get(topic, H),
%   From1 = maps:get(from_id, H),
%   Uuid1 = maps:get(uuid, H),
%   Message = hoolva_chat_utils:decrypt(P),
%   Data = emqx_message:make(From1,Qos1,Topic,Message),
%   emqx:publish(Data),
%   put_chat(#{uuid => Uuid1, status => <<"delivered">>}),
%   io:format("~nmessage send to ~p~n",[Topic]).



retained(Topic,B) ->
  % A = get_chat(#{topic => Topic, status => <<"undelivered">>}),
  case B of
    [] ->
      ok;
    _ ->
      messages(Topic,B)
    end.

messages(_,[]) ->
  ok;
messages(Topic,[H|T]) ->
  % Message = element(5,H),
  Message_id = maps:get(message_id, H),
  Qos = maps:get(qos, H),
  From = maps:get(from_id, H),

  P = maps:get(message, H),
  Message = hoolva_chat_utils:decrypt(P),

  % Flags = maps:get(flags, H),
  Headers = maps:get(headers, H),
  Time = maps:get(time, H),

  Data = {message,Message_id,Qos,From,#{},Headers,Topic,Message,Time},
  emqx:publish(Data),
  io:format("~nmessage send to ~p~n",[Topic]),
  % io:format("~nData - ~p ~n",[Data]),
  put_chat(#{message_id => Message_id, status => <<"delivered">>}),
  % io:format("~nput chat -> ~p~n",[Response]),
  messages(Topic,T).


