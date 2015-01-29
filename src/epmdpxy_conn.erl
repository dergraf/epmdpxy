-module(epmdpxy_conn).
-include("epmdpxy.hrl").

-behaviour(gen_server).

%% API
-export([start_link/0,
         hand_over_socket/2]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {socket, buffer= <<>>}).

-define(ALIVE2_REQ, 120).
-define(ALIVE2_RESP, 121).
-define(PORT_PLEASE2_REQ, 122).
-define(PORT2_RESP, 119).
-define(NAMES_REQ, 110).
-define(DUMP_REQ, 100).

-define(OK, 0).

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
    gen_server:start_link(?MODULE, [], []).

hand_over_socket(Pid, Socket) ->
    gen_server:cast(Pid, {hand_over_socket, Socket}).

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
handle_cast({hand_over_socket, Socket}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {noreply, State#state{socket=Socket}}.

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
handle_info({tcp, Socket, Data}, #state{socket=Socket, buffer=Buffer} = State) ->
    case parse(Socket, <<Buffer/binary, Data/binary>>) of
        {ok, NewBuffer} ->
            inet:setopts(Socket, [{active, once}]),
            {noreply, State#state{buffer=NewBuffer}};
        stop ->
            ok = gen_tcp:close(Socket),
            {stop, normal, State}
    end;
handle_info({tcp_closed, Socket}, #state{socket=Socket} = State) ->
    {stop, normal, State};
handle_info({tcp_error, Socket, Reason}, #state{socket=Socket} = State) ->
    {stop, Reason, State}.

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
parse(Socket, <<Len:16, Rest/binary>>) when size(Rest) >= Len ->
    <<Req:Len/binary, Rest1/binary>> = Rest,
    case parse_req(Socket, Req) of
        ok -> {ok, Rest1};
        stop -> stop;
        error ->
            io:format("got unknown request ~p~n", [Req]),
            stop
    end;
parse(_, B) -> {ok, B}.


parse_req(Socket, <<?ALIVE2_REQ:8, PortNo:16, NodeType:8, Protocol:8,
                    HighestVersion:16, LowestVersion:16, NLen:16,
                    Rest/binary>>) ->
    <<NodeName:NLen/binary, _:16, Extra/binary>> = Rest,
    case epmdpxy_reg:register_node(PortNo, NodeType, Protocol, HighestVersion,
                                   LowestVersion, NodeName, Extra) of
        ok ->
            gen_tcp:send(Socket, <<?ALIVE2_RESP, ?OK, 0, 2>>),
            ok;
        {error, already_registered} ->
            gen_tcp:send(Socket, <<?ALIVE2_RESP, 1, 0, 0>>),
            stop
    end;
parse_req(Socket, <<?PORT_PLEASE2_REQ:8, NodeName/binary>>) ->
    case epmdpxy_reg:get_node(NodeName) of
        {ok, #node{node_type=NodeType,
                   proxy_port_no=ProxyPortNo,
                   protocol=Protocol,
                   highest_version=HighestVersion,
                   lowest_version=LowestVersion,
                   node_name=NodeName,
                   extra=Extra}} ->
            gen_tcp:send(Socket, <<?PORT2_RESP:8, ?OK,
                                   ProxyPortNo:16,
                                   NodeType:8,
                                   Protocol:8,
                                   HighestVersion:16,
                                   LowestVersion:16,
                                   (size(NodeName)):16,
                                   NodeName/binary,
                                   (size(Extra)):16, Extra/binary>>),
            stop;
        {error, not_found} ->
            gen_tcp:send(Socket, <<?PORT2_RESP:8, 1>>),
            stop
    end;
parse_req(Socket, <<?NAMES_REQ:8>>) ->
    {ok, EpmdPort} = application:get_env(epmdpxy, port),
    Acc = [<<EpmdPort:32>>],
    Res =
    epmdpxy_reg:fold(
      fun(#node{node_name=NodeName, port_no=Port}, Buffer) ->
              S = io_lib:format("name ~ts at port ~p~n", [NodeName, Port]),
              [S|Buffer]
      end, Acc),
    gen_tcp:send(Socket, lists:reverse(Res)),
    stop;

parse_req(_, _) -> error.
