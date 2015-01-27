-module(epmdpxy_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    {ok, EpmdPort} = application:get_env(epmdpxy, port),
    {ok, _} = ranch:start_listener(epmd, 1,
                                   ranch_tcp,
                                   [{port, EpmdPort}, {reuseaddr, true}],
                                   epmd_protocol, []),
    epmdpxy_sup:start_link().

stop(_State) ->
    ok.
