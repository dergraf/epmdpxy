-module(epmdpxy_netsplit_tests).
-export([start_node/0,
         proxy/0]).
-include_lib("eunit/include/eunit.hrl").

-define(NR_OF_NODES, 5).
-define(NET_TICK_TIME, 5). % 5 seconds

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Setup Functions
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
run_test_() ->
    {timeout, ?NET_TICK_TIME * 3,
     {setup,
      fun() ->
              epmdpxy:start(epmdpxy_test, epmd_port()),
              net_kernel:set_net_ticktime(?NET_TICK_TIME, ?NET_TICK_TIME),
              Hosts = hosts(),
              Ns = start_slaves(Hosts),
              try
                  [ok = rpc:call(Node, ?MODULE, start_node, [])
                   || Node <- Ns],
                  timer:sleep(?NET_TICK_TIME),
                  Ns
              catch
                  _:R ->
                      stop_slaves(Ns),
                      exit(R)
              end
      end,
      fun(Ns) ->
              stop_slaves(Ns)
      end,
      fun(Ns) ->
              {timeout, ?NET_TICK_TIME * 2,
               [?_test(netsplit_test(Ns))]
              }
      end
     }}.


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Actual Tests
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
netsplit_test(Nodes) ->
    ok = check_connected(Nodes, 0),
    io:format(user, "Nodes ~p are fully connected~n", [Nodes]),

    %% Create Partitions
    Size = ?NR_OF_NODES div 2,
    Island1 = lists:sublist(Nodes, Size),
    Island2 = Nodes -- Island1,
    io:format(user, "Create two partitions ~p and ~p~n", [Island1, Island2]),
    epmdpxy:cut_cables(Island1, Island2),
    io:format(user, "Sleep ~p seconds (net_ticktime)~n", [?NET_TICK_TIME]),
    timer:sleep(?NET_TICK_TIME * 1000),
    io:format(user, "Check network partitions~n", []),
    check_connected(Island1, 0),
    check_connected(Island2, 0),
    io:format(user, "Fix network partitions~n", []),
    epmdpxy:fix_cables(Island1, Island2),
    ok = check_connected(Nodes, 0).


check_connected([N1, N2|Rest] = Nodes, I) when length(Nodes) < I ->
    %% ensure all nodes are connected
    [N2|Rest] = call_proxy(N1, erlang, nodes, []) -- [node()],
    check_connected([N2|Rest] ++ [N1], I + 1);
check_connected(_ , _) -> ok.




%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% Internal
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

hosts() ->
    [list_to_atom("node"++integer_to_list(I))
     || I <- lists:seq(1, ?NR_OF_NODES)].

start_node() ->
    net_kernel:set_net_ticktime(?NET_TICK_TIME, ?NET_TICK_TIME),
    ok.

%% from uwiger/locks
-define(PROXY, epmdpxy_test_proxy).
proxy() ->
    register(?PROXY, self()),
    process_flag(trap_exit, true),
    proxy_loop().

proxy_loop() ->
    receive
        {From, Ref, apply, M, F, A} ->
            From ! {Ref, (catch apply(M,F,A))};
        _ ->
            ok
    end,
    proxy_loop().

%proxy_multicall(Ns, M, F, A) ->
%    [call_proxy(N, M, F, A) || N <- Ns].
%
call_proxy(N, M, F, A) ->
    Ref = erlang:monitor(process, {?PROXY, N}),
    {?PROXY, N} ! {self(), Ref, apply, M, F, A},
    receive
        {'DOWN', Ref, _, _, Reason} ->
            error({proxy_died, N, Reason});
        {Ref, Result} ->
            Result
    after 1000 ->
              error(proxy_call_timeout)
    end.

start_slaves(Ns) ->
    Nodes = [start_slave(N) || N <- Ns],
    Nodes.

start_slave(Name) ->
    {Pa, Pz} = paths(),
    Paths = "-pa ./ -pz ../ebin" ++
    lists:flatten([[" -pa " ++ Path || Path <- Pa],
                   [" -pz " ++ Path || Path <- Pz]]),
    {ok, Node} = ct_slave:start(host(), Name, [{erl_flags, Paths},
                                               {monitor_master, true}]),
    spawn(Node, ?MODULE, proxy, []),
    Node.

stop_slaves(Ns) ->
    [ok = stop_slave(N) || N <- Ns],
    ok.

stop_slave(N) ->
    try erlang:monitor_node(N, true) of
        true ->
            rpc:call(N, erlang, halt, []),
            receive
                {nodedown, N} -> ok
            after 10000 ->
                      erlang:error(slave_stop_timeout)
            end
    catch
        error:badarg ->
            ok
    end.

paths() ->
    Path = code:get_path(),
    {ok, [[Root]]} = init:get_argument(root),
    {Pas, Rest} = lists:splitwith(fun(P) ->
                                          not lists:prefix(Root, P)
                                  end, Path),
    Pzs = lists:filter(fun(P) ->
                               not lists:prefix(Root, P)
                       end, Rest),
    {Pas, Pzs}.

host() ->
    {ok, HostName} = inet:gethostname(),
    list_to_atom(HostName).

epmd_port() ->
    {ok, [[StrEPMD_PORT]]} = init:get_argument(epmd_port),
    list_to_integer(StrEPMD_PORT).
