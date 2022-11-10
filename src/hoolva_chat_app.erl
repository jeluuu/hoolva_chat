-module(hoolva_chat_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    % Env = application:get_all_env(hoolva_chat),
    {ok, Sup} = hoolva_chat_sup:start_link(),
    hoolva_chat:load(application:get_all_env()),
    % hoolva_chat:load(Env),
    {ok, Sup}.

stop(_State) ->
    hoolva_chat:unload().