-module(epmdpxy_session_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_session/2,
         connection_created/3,
         connection_deleted/2,
         cut_cables/2,
         fix_cables/2]).

%% Supervisor callbacks
-export([init/1]).

-define(CHILD(Id, Mod, Type, Args), {Id, {Mod, start_link, Args},
                                     permanent, 5000, Type, [Mod]}).

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

start_session(Name, Port) ->
    {ok, Pid} = supervisor:start_child(?MODULE, [Name, Port]),
    ProxyPort = epmdpxy_session:accept(Pid),
    ProxyPort.

connection_created(SessionPid, DownstreamNode, UpstreamNode) ->
    ets:insert(?MODULE, {{DownstreamNode, UpstreamNode}, SessionPid}).

connection_deleted(DownstreamNode, UpstreamNode) ->
    ets:match_delete(?MODULE, {DownstreamNode, UpstreamNode}).

cut_cables(DownstreamNodes, UpstreamNodes) ->
    map_cables(true, DownstreamNodes, UpstreamNodes).

fix_cables(DownstreamNodes, UpstreamNodes) ->
    map_cables(false, DownstreamNodes, UpstreamNodes).

map_cables(Cut, [DownStreamNode|DownstreamNodes], UpstreamNodes) ->
    lists:foreach(
      fun(N) ->
              case ets:lookup(?MODULE, {DownStreamNode, N}) of
                  [] -> ok;
                  [{_, SessionPid}] when Cut ->
                      epmdpxy_session:cut_cable(SessionPid);
                  [{_, SessionPid}] ->
                      epmdpxy_session:fix_cable(SessionPid)
              end
      end, UpstreamNodes),
    map_cables(Cut, DownstreamNodes, UpstreamNodes);
map_cables(_, [], _) -> ok.

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
    ets:new(?MODULE, [named_table, public]),
    {ok, {{simple_one_for_one, 0, 1}, [?CHILD(epmdpxy_session, epmdpxy_session, worker, [])]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
