-module(epmdpxy_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_session/2,
         connection_created/3,
         connection_deleted/3,
         status/0]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
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

start_session(UpstreamNode, Port) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [UpstreamNode, Port]),
    ProxyPort = epmdpxy_session:accept(Pid),
    ProxyPort.

connection_created(SessionPid, DownstreamNode, UpstreamNode) ->
    epmdpxy_splitter:add_cable(DownstreamNode, UpstreamNode, SessionPid).

connection_deleted(SessionPid, DownstreamNode, UpstreamNode) ->
    epmdpxy_splitter:delete_cable(DownstreamNode, UpstreamNode, SessionPid).

status() ->
    [epmdpxy_session:status(Pid)
     || {_, Pid, _, _} <- supervisor:which_children(?MODULE), is_pid(Pid)].

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
    {ok, {{simple_one_for_one, 0, 5},
          [?CHILD(epmdpxy_session, epmdpxy_session, worker, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
