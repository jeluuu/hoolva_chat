-module(hoolva_chat_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).
    % fcm:start(push,"806059320156").

init([]) ->
    % {ok, { {one_for_all, 0, 1}, []} }.
    SupFlags = #{strategy => one_for_one,
               intensity => 10,
               period => 10},
    ChildSpecs = [ #{id => hoolva_chat_actions, start => {hoolva_chat_actions, start_link, []}} ],
    {ok, {SupFlags, ChildSpecs}}.