ovsdb
=====
[![hex.pm version](https://img.shields.io/hexpm/v/ovsdb.svg)](https://hex.pm/packages/ovsdb)
[![Build Status](https://travis-ci.org/vasu-dasari/erlang-ovsdb.svg?branch=master)](https://travis-ci.org/vasu-dasari/erlang-ovsdb)
[![Code Coverage](https://codecov.io/gh/vasu-dasari/erlang-ovsdb/branch/master/graph/badge.svg)](https://codecov.io/gh/vasu-dasari/erlang-ovsdb/branch/master)
[![License: Apache](https://img.shields.io/badge/license-APACHE-blue.svg)](LICENSE)


An Erlang library for supporting OVSDB protocol as defined in [RFC7047](https://tools.ietf.org/html/rfc7047). APIs are inspired by Ryu's [OVSDB library](https://ryu.readthedocs.io/en/latest/library_ovsdb.html).

QuckStart
---------

```sh
# clone
$ git clone git://github.com/vasu-dasari/erlang-ovsdb.git
$ cd erlang-ovsdb

# Compile and run
$ make run
```
There is an option to try out the library against OVS instance. Project has a [docker/docker-compose.yml](docker/docker-compose.yml) which defines the docker network that can setup to demonstrate this.

```sh
# make up       # Brings up container network
# make run      # Compile and run ovsdb, brings up Erlang shell

# enable ovsdb manager in ovs1
# make connect ovs1     # Brings up ovs1 console
root@ovs1:~# ovs-vsctl set-manager ptcp:6640

# Tu run various API commands agains OVS container ovs1,
(ovsdb@ovsdb)1> ovsdb_utils:unit_test_ovs().

```
Look at the code `ovsdb_utils:unit_test_ovs()` for more information.

Usage Example
-------------


API
---

See [EDoc Document](doc)

License
-------

This library is released under the Apache License.

See the [COPYING](COPYING) file for full license information.

Acknowledgement
---------------
Some ideas for implementing the library were take from [shun159/eovsdb
](https://github.com/shun159/eovsdb). 
