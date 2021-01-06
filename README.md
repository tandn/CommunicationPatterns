#### A distributed cache that use processes to hold key,value pair.
##### Implemented by following chapters 4,6,7,8,9,11 in "Erlang and OTP in Action"


#### Compile: rebar3
 - sc: the cache
 - rd: resource discovery for connecting a new cache node with already-up cached nodes in a cluster
 - ti: tcp interface to interact with the cache via tcp (e.g., telnet)
