-module(epmdpxy_conn_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_conn/1]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Mod, Type, Args), {Mod, {Mod, start_link, Args},
                                     temporary, 5000, Type, [Mod]}).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_conn(Sock) ->
    {ok, Pid} = supervisor:start_child(?MODULE, []),
    ok = gen_tcp:controlling_process(Sock, Pid),
    ok = epmdpxy_conn:hand_over_socket(Pid, Sock),
    {ok, Pid}.

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{simple_one_for_one, 0, 5}, [?CHILD(epmdpxy_conn, worker, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
