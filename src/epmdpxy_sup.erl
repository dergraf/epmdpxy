-module(epmdpxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {ok, { {one_for_one, 5, 10}, [?CHILD(epmdpxy_listener, worker),
                                  ?CHILD(epmdpxy_conn_sup, supervisor),
                                  ?CHILD(epmdpxy_splitter, worker),
                                  ?CHILD(epmdpxy_session_sup, supervisor),
                                  ?CHILD(epmdpxy_reg, worker)]} }.

