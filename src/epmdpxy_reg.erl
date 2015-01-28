-module(epmdpxy_reg).
-include("epmdpxy.hrl").
-behaviour(gen_server).

%% API
-export([start_link/0,
         register_node/7,
         get_node/1,
         fold/2]).

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

register_node(PortNo, NodeType, Protocol, HighestVersion,
              LowestVersion, NodeName, Extra) ->
    case get_node_(NodeName) of
        {error, not_found} ->
            Node = #node{port_no=PortNo,
                         node_type=NodeType,
                         protocol=Protocol,
                         highest_version=HighestVersion,
                         lowest_version=LowestVersion,
                         node_name=NodeName,
                         extra=Extra,
                         pid=self()},
            gen_server:call(?MODULE, {register_node, Node});
        {ok, _} ->
            {error, already_registered}
    end.

fold(FoldFun, FoldAcc) ->
    ets:foldl(FoldFun, FoldAcc, ?MODULE).

get_node(NodeName) ->
    case get_node_(NodeName) of
        {ok, #node{port_no=Port} = Node} ->
            ProxyPort = epmdpxy_session_sup:start_session(NodeName, Port),
            {ok, Node#node{proxy_port_no=ProxyPort}};
        {error, not_found} ->
            {error, not_found}
    end.

get_node_(NodeName) ->
    gen_server:call(?MODULE, {get_node, NodeName}).

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
    ets:new(?MODULE, [named_table, public, {keypos, 2}]),
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
handle_call({register_node, #node{pid=Pid} = Node}, _From, State) ->
    MRef = monitor(process, Pid),
    ets:insert(?MODULE, Node#node{monitor=MRef}),
    {reply, ok, State};
handle_call({get_node, NodeName}, _From, State) ->
    Reply =
    case ets:match_object(?MODULE, #node{node_name=NodeName, _='_'}) of
        [] -> {error, not_found};
        [Node] ->{ok, Node}
    end,
    {reply, Reply, State};
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

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
handle_info({'DOWN', _, process, Pid, _}, State) ->
    ets:delete(?MODULE, Pid),
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

