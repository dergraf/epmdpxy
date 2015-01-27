-module(epmd_protocol).
-include("epmdpxy.hrl").
-behaviour(ranch_protocol).
-export([start_link/4]).
-export([init/4]).

-define(ALIVE2_REQ, 120).
-define(ALIVE2_RESP, 121).
-define(PORT_PLEASE2_REQ, 122).
-define(PORT2_RESP, 119).
-define(NAMES_REQ, 110).
-define(DUMP_REQ, 100).

-define(OK, 0).

start_link(Ref, Socket, Transport, Opts) ->
    Pid = spawn_link(?MODULE, init, [Ref, Socket, Transport, Opts]),
    {ok, Pid}.

init(Ref, Socket, Transport, _Opts = []) ->
    ok = ranch:accept_ack(Ref),
    loop(Socket, Transport, <<>>).

loop(Socket, Transport, Buffer) ->
    Transport:setopts(Socket, [{active, once}]),
    receive
        {tcp, Socket, Data} ->
            case parse(Transport, Socket, <<Buffer/binary, Data/binary>>) of
                {ok, NewBuffer} ->
                    loop(Socket, Transport, NewBuffer);
                stop ->
                    ok = Transport:close(Socket)
            end;
        _ ->
            ok = Transport:close(Socket)
    end.

parse(Transport, Socket, <<Len:16, Rest/binary>>) when size(Rest) >= Len ->
    <<Req:Len/binary, Rest1/binary>> = Rest,
    case parse_req(Transport, Socket, Req) of
        ok -> {ok, Rest1};
        stop -> stop;
        error ->
            io:format("got unknown request ~p~n", [Req]),
            stop
    end;
parse(_, _, B) -> {ok, B}.


parse_req(Transport, Socket, <<?ALIVE2_REQ:8, PortNo:16, NodeType:8, Protocol:8,
                               HighestVersion:16, LowestVersion:16, NLen:16,
                               Rest/binary>>) ->
    <<NodeName:NLen/binary, _:16, Extra/binary>> = Rest,
    case epmdpxy:register_node(PortNo, NodeType, Protocol, HighestVersion,
                            LowestVersion, NodeName, Extra) of
        ok ->
            Transport:send(Socket, <<?ALIVE2_RESP, ?OK, 0, 2>>),
            ok;
        {error, already_registered} ->
            Transport:send(Socket, <<?ALIVE2_RESP, 1, 0, 0>>),
            stop
    end;
parse_req(Transport, Socket, <<?PORT_PLEASE2_REQ:8, NodeName/binary>>) ->
    case epmdpxy:get_node(NodeName) of
        {ok, #node{node_type=NodeType,
                   proxy_port_no=ProxyPortNo,
                   protocol=Protocol,
                   highest_version=HighestVersion,
                   lowest_version=LowestVersion,
                   node_name=NodeName,
                   extra=Extra}} ->
            Transport:send(Socket, <<?PORT2_RESP:8, ?OK,
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
            Transport:send(Socket, <<?PORT2_RESP:8, 1>>),
            stop
    end;
parse_req(Transport, Socket, <<?NAMES_REQ:8>>) ->
    {ok, EpmdPort} = application:get_env(epmdpxy, port),
    Acc = [<<EpmdPort:32>>],
    Res =
    epmdpxy:fold(
      fun(#node{node_name=NodeName, port_no=Port}, Buffer) ->
              S = io_lib:format("name ~ts at port ~p~n", [NodeName, Port]),
              [S|Buffer]
      end, Acc),
    Transport:send(Socket, lists:reverse(Res)),
    stop;

parse_req(_, _, _) -> error.
