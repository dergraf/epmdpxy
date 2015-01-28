-module(epmdpxy).
-export([start/0,
         status/0,
         cut_cables/2,
         fix_cables/2]).

start() ->
    application:ensure_all_started(epmdpxy).

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
