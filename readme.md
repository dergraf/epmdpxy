# EPMDPXY - A Proxy for the Erlang Port Mapper Deamon [![Build Status](https://travis-ci.org/dergraf/epmdpxy.svg)](https://travis-ci.org/dergraf/epmdpxy)

EPMDPXY simulates the basic functionality of EPMD, just enough to make a local Erlang cluster work. With one exception though, instead of replying the listener port when handling <code>PORT_PLEASE2_REQ</code>, it spawns an internal listener and replies the new (random) port number instead. The newly spawned listener process accepts one single connection and connects to the 'real' listener port of the remote node, acting as a proxy between the two Erlang nodes.

### EPMD

If a distributed Erlang node is started with the <code>-name</code> or <code>-sname</code> argument it will first try to connect to the EPMD. In case the EPMD is not running on that local machine yet it gets started as a separate OS process. The EPMD normally listens on the <code>4369</code> TCP port, this can be adjusted using the environment variable <code>ERL_EPMD_PORT</code>. A distributed Erlang node connects to the EPMD and registers its node name together with a random listener port, that is used for accepting connections from other nodes in the cluster (<code>ALIVE2_REQ</code>). If a node wants to communicate with an other cluster node it hasn't seen yet, it contacts the EPMD and asks for the listening port of the other node (<code>PORT_PLEASE2_REQ</code>).

It's all on [Distributed Erlang](http://www.erlang.org/doc/reference_manual/distributed.html) and [Erlang Distribution Protocol](http://www.erlang.org/doc/apps/erts/erl_dist_protocol.html)

## Starting EPMDPXY

Open a new Erlang shell, without the <code>-name</code> or <code>-sname</code> arguments.

```erlang
%% start the epmdpxy listening on the default EPMD port 4369.
%% be sure that no other EPMD is running on this port.
epmdpxy:start(). 
```

If you prefer to use a different EPMD port use:

```erlang
%% start the epmdpxy listening on a non standard port.
epmdpxy:start(43690). 
```
If you go this way, you have to set the <code>ERL_EPMD_PORT=43690</code> environment variable before starting
your cluster nodes.

### Simulating Network Partitions

EPMDPXY currently only implements one feature, namely the simulation of network partitions in an Erlang cluster.
Assuming your cluster consists of five nodes <code>node1, node2, node3, node4, node5</code>. Simulating a
netsplit resulting in two islands <code>[node1, node2]</code> and <code>[node3, node4, node5]</code> can be done
using:

```erlang
epmdpxy:cut_cables([node1, node2], [node3, node4, node5]).
```

In this situation the proxy blocks all the traffic between the two islands.

To fix the network partition call:

```erlang
epmdpxy:fix_cables([node1, node2], [node3, node4, node5]).
```

## Remarks

- it's pretty alpha.. 
- we'll be using it at Erlio GmbH, so it will eventually mature
- of course this is not a replacement for real hardware damage
