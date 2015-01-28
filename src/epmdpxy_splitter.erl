-module(epmdpxy_splitter).

-behaviour(gen_server).

%% API
-export([start_link/0,
         add_cable/3,
         delete_cable/3,
         cut_cables/2,
         fix_cables/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

add_cable(NodeA, NodeB, SessionPid) ->
    SNodeA = ensure_sname(NodeA),
    SNodeB = ensure_sname(NodeB),
    gen_server:call(?MODULE, {add_cable, SNodeA, SNodeB, SessionPid}).

delete_cable(NodeA, NodeB, SessionPid) ->
    SNodeA = ensure_sname(NodeA),
    SNodeB = ensure_sname(NodeB),
    gen_server:call(?MODULE, {del_cable, SNodeA, SNodeB, SessionPid}).

cut_cables(IslandA, IslandB) ->
    play_with_cables(true, IslandA, IslandB).

fix_cables(IslandA, IslandB) ->
    play_with_cables(false, IslandA, IslandB).

play_with_cables(Cut, IslandA, IslandB) ->
    gen_server:call(?MODULE, {play_with_cables, Cut, IslandA, IslandB}).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    ets:new(epmdpxy_cables, [named_table, bag]),
    ets:new(epmdpxy_cuts, [named_table]),
    {ok, #state{}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call({add_cable, NodeA, NodeB, SessionPid}, _From, State) ->
    Key = lists:sort([NodeA, NodeB]),
    ets:insert(epmdpxy_cables, {Key, SessionPid}),
    IsActive = [] == ets:lookup(epmdpxy_cuts, Key),
    {reply, IsActive, State};
handle_call({del_cable, NodeA, NodeB, SessionPid}, _From, State) ->
    Key = lists:sort([NodeA, NodeB]),
    ets:delete_object(epmdpxy_cables, {Key, SessionPid}),
    {reply, ok, State};
handle_call({play_with_cables, Cut, IslandA, IslandB}, _From, State) ->
    Segments = map_cables(IslandA, IslandB, []),
    [case Cut of
         true ->
             epmdpxy_session:cut_cable(SessionPid),
             ets:insert(epmdpxy_cuts, {Key, cut});
         false ->
             epmdpxy_session:fix_cable(SessionPid),
             ets:delete(epmdpxy_cuts, Key)
     end || {Key, SessionPid} <- Segments],
    {reply, ok, State}.



%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
ensure_sname(Name) when is_binary(Name) ->
    [SName|_] = re:split(Name, "@"),
    SName;
ensure_sname(undefined) ->
    undefined.

map_cables([DownStreamNode|DownstreamNodes], UpstreamNodes, Acc) ->
    SDownStreamNode = ensure_sname(DownStreamNode),
    map_cables(
      DownstreamNodes,
      UpstreamNodes,
      lists:foldl(
        fun(N, AccAcc) ->
                Key = lists:sort([SDownStreamNode, ensure_sname(N)]),
                lists:foldl(fun({_, SessionPid}, AccAccAcc) ->
                                    [{Key, SessionPid}|AccAccAcc]
                            end, AccAcc, ets:lookup(epmdpxy_cables, Key))
        end, Acc, UpstreamNodes));
map_cables([], _, Acc) -> Acc.
