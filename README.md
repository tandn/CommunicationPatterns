#### A distributed cache that uses processes to hold {Key,Value} pairs.

### API: sc:insert/2 (sc:update/2), sc:lookup/1, sc:delete/1

- Distribution relies on Mnesia - mapping between Key and Pid
- Implemented by following chapters 4,6,7,8,9,11 in "Erlang and OTP in Action".

#### Compile: rebar3
 - sc: cache implementation + event logger + API
 - rd: utility that connects a new cache node (VM) with already-up cached nodes in a cluster
 - ti: a tcp interface that allows to interact with the cache via tcp (e.g., telnet)
