-module(epmdpxy).
-export([start/0,
         start/1,
         start/2,
         status/0,
         cut_cables/2,
         fix_cables/2]).

start() ->
    EPMD_PORT =
    case os:getenv("ERL_EPMD_PORT") of
        false ->
            {ok, DefaultPort} = application:get_env(epmdpxy, port),
            DefaultPort;
        StrPort ->
            list_to_integer(StrPort)
    end,
    start(EPMD_PORT).

start(EPMD_PORT) ->
    application:load(epmdpxy),
    application:set_env(epmdpxy, port, EPMD_PORT),
    application:ensure_all_started(epmdpxy).

start(NodeName, EPMD_PORT) when is_atom(NodeName) and is_integer(EPMD_PORT) ->
    %% be sure that you either started the vm with erl -epmd_prot EPMD_PORT
    %% or you have exported the enviroment variable ERL_EPMD_PORT
    StrEPMD_PORT = integer_to_list(EPMD_PORT),
    {ok, [[StrEPMD_PORT]]} = init:get_argument(epmd_port),
    start(EPMD_PORT),
    NodeNameType =
    case re:split(atom_to_list(NodeName), "@") of
        [_] -> shortnames;
        [_,_] -> longnames
    end,
    {ok, _} = net_kernel:start([NodeName, NodeNameType]),
    ok.


status() ->
    epmdpxy_session_sup:status().

cut_cables(Island1, Island2) ->
    epmdpxy_splitter:cut_cables(sanitize(Island1),
                                sanitize(Island2)).

fix_cables(Island1, Island2) ->
    epmdpxy_splitter:fix_cables(sanitize(Island1),
                                sanitize(Island2)).

sanitize(Nodes) ->
    sanitize(Nodes, []).

sanitize([Node|Rest], Acc) when is_atom(Node) ->
    sanitize(Rest, [atom_to_binary(Node, latin1)|Acc]);
sanitize([Node|Rest], Acc) when is_list(Node) ->
    sanitize(Rest, [list_to_binary(Node)|Acc]);
sanitize([Node|Rest], Acc) when is_binary(Node) ->
    sanitize(Rest, [Node|Acc]);
sanitize([], Acc) -> Acc.
