-module(epmdpxy_session).

-behaviour(gen_server).

%% API
-export([start_link/2,
         accept/1,
         cut_cable/1,
         fix_cable/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {status=ok,
                downstream_name,
                upstream_name,
                upstream_port,
                listen_socket,
                downstream_socket,
                upstream_socket}).

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
start_link(Name, Port) ->
    gen_server:start_link(?MODULE, [Name, Port], []).

accept(Pid) ->
    gen_server:call(Pid, accept).

cut_cable(Pid) ->
    gen_server:call(Pid, cut_cable).

fix_cable(Pid) ->
    gen_server:call(Pid, fix_cable).


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
init([Name, Port]) ->
    {ok, ListenSocket} = gen_tcp:listen(0, [binary]),
    {ok, #state{
            upstream_name=Name,
            upstream_port=Port,
            listen_socket=ListenSocket}}.

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
handle_call(accept, From, #state{listen_socket=ListenSocket} = State) ->
    {ok, ListenPort} = inet:port(ListenSocket),
    gen_server:reply(From, ListenPort),
    {ok, Socket} = gen_tcp:accept(ListenSocket),
    {noreply, State#state{downstream_socket=Socket}};
handle_call(cut_cable, _From, #state{downstream_socket=S,
                                     upstream_socket=UpS} = State) ->
    inet:setopts(S, [{active, false}]),
    inet:setopts(UpS, [{active, false}]),
    {reply, ok, State#state{status=cut}};
handle_call(fix_cable, _From, #state{downstream_socket=S,
                                     upstream_socket=UpS} = State) ->
    inet:setopts(S, [{active, true}]),
    inet:setopts(UpS, [{active, true}]),
    {reply, ok, State#state{status=ok}};
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
handle_info({tcp, S, Data}, #state{downstream_name=Name,
                                   downstream_socket=S,
                                   upstream_name=UpName} = State) ->
    NewState =
    case Name of
        undefined ->
            State#state{downstream_name=parse_name(UpName, Data)};
        _ ->
            State
    end,
    case maybe_connect(NewState) of
        {ok, #state{upstream_socket=UpS} = NewNewState} ->
            gen_tcp:send(UpS, Data),
            {noreply, NewNewState};
        {error, Reason} ->
            gen_tcp:close(S),
            {stop, Reason, NewState}
    end;
handle_info({tcp, UpS, Data}, #state{downstream_socket=S,
                                     upstream_socket=UpS} = State) ->
    gen_tcp:send(S, Data),
    {noreply, State};
handle_info({tcp_closed, _}, State) ->
    {stop, normal, State};
handle_info({tcp_error, _, Error}, State) ->
    {stop, Error, State}.



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
terminate(_Reason, #state{upstream_name=UpstreamName,
                          downstream_name=DownstreamName}) ->
    epmdpxy_session_sup:connection_deleted(DownstreamName, UpstreamName),
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
maybe_connect(#state{upstream_port=Port, upstream_socket=undefined} = State) ->
    case gen_tcp:connect({127,0,0,1}, Port, [binary, {active, true}]) of
        {ok, Socket} ->
            {ok, State#state{upstream_socket=Socket}};
        {error, Reason} ->
            {error, Reason}
    end;
maybe_connect(State) ->
    {ok, State}.

parse_name(UpstreamName, <<_Len:16, "n", _Version:16, _Flags:32,
                           DownstreamName/binary>>) ->
    process_flag(trap_exit, true),
    epmdpxy_session_sup:connection_created(self(), DownstreamName, UpstreamName),
    DownstreamName;
parse_name(_, _) -> undefined.
