

# Module ovsdb_client #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

OVSDB Client Module.

__Behaviours:__ [`gen_server`](gen_server.md).

__Authors:__ Vasu Dasari.

<a name="description"></a>

## Description ##
This Module is the primary interface to OVSDB library

<a name="types"></a>

## Data Types ##




### <a name="type-db_name">db_name()</a> ###


<pre><code>
db_name() = iolist()
</code></pre>




### <a name="type-db_table">db_table()</a> ###


<pre><code>
db_table() = iolist()
</code></pre>




### <a name="type-dst">dst()</a> ###


<pre><code>
dst() = pid() | port() | (RegName::atom()) | {RegName::atom(), Node::node()}
</code></pre>




### <a name="type-json_value">json_value()</a> ###


<pre><code>
json_value() = <a href="jsone.md#type-json_value">jsone:json_value()</a>
</code></pre>




### <a name="type-ovsdb_ops">ovsdb_ops()</a> ###


<pre><code>
ovsdb_ops() = map()
</code></pre>




### <a name="type-rpc_return">rpc_return()</a> ###


<pre><code>
rpc_return() = {ok, term()} | {error, {term(), term()}} | not_connected
</code></pre>




### <a name="type-select">select()</a> ###


<pre><code>
select() = iolist() | map()
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#start-2">start/2</a></td><td></td></tr><tr><td valign="top"><a href="#list_dbs-1">list_dbs/1</a></td><td>Lists available databases.</td></tr><tr><td valign="top"><a href="#list_dbs-0">list_dbs/0</a></td><td>Equivalent to <a href="#list_dbs-1"><tt>list_dbs(ovsdb_client)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_schema-2">get_schema/2</a></td><td>Get database schema.</td></tr><tr><td valign="top"><a href="#get_schema-1">get_schema/1</a></td><td>Equivalent to <a href="#get_schema-2"><tt>get_schema(ovsdb_client, DbName)</tt></a>.</td></tr><tr><td valign="top"><a href="#transaction-3">transaction/3</a></td><td>Perform OVSDB Transaction.</td></tr><tr><td valign="top"><a href="#transaction-2">transaction/2</a></td><td>Equivalent to <a href="#transaction-3"><tt>transaction(ovsdb_client, DbName, Operation)</tt></a>.</td></tr><tr><td valign="top"><a href="#cancel-3">cancel/3</a></td><td>Cancel Transaction.</td></tr><tr><td valign="top"><a href="#cancel-2">cancel/2</a></td><td>Equivalent to <a href="#cancel-3"><tt>cancel(ovsdb_client, DbName, Operation)</tt></a>.</td></tr><tr><td valign="top"><a href="#monitor-4">monitor/4</a></td><td>Monitor.</td></tr><tr><td valign="top"><a href="#monitor-3">monitor/3</a></td><td>Equivalent to <a href="#monitor-4"><tt>monitor(ovsdb_client, Id, DbName, Select)</tt></a>.</td></tr><tr><td valign="top"><a href="#monitor_cancel-2">monitor_cancel/2</a></td><td>Cancel Monitor Operation.</td></tr><tr><td valign="top"><a href="#monitor_cancel-1">monitor_cancel/1</a></td><td>Equivalent to <a href="#monitor_cancel-2"><tt>monitor_cancel(ovsdb_client, Id)</tt></a>.</td></tr><tr><td valign="top"><a href="#lock-2">lock/2</a></td><td>Lock Database.</td></tr><tr><td valign="top"><a href="#lock-1">lock/1</a></td><td>Equivalent to <a href="#lock-2"><tt>lock(ovsdb_client, Id)</tt></a>.</td></tr><tr><td valign="top"><a href="#steal-2">steal/2</a></td><td>Steal lock.</td></tr><tr><td valign="top"><a href="#steal-1">steal/1</a></td><td>Equivalent to <a href="#steal-2"><tt>steal(ovsdb_client, Id)</tt></a>.</td></tr><tr><td valign="top"><a href="#unlock-2">unlock/2</a></td><td>Unlock database.</td></tr><tr><td valign="top"><a href="#unlock-1">unlock/1</a></td><td>Equivalent to <a href="#unlock-2"><tt>unlock(ovsdb_client, Id)</tt></a>.</td></tr><tr><td valign="top"><a href="#echo-1">echo/1</a></td><td>Echo.</td></tr><tr><td valign="top"><a href="#echo-0">echo/0</a></td><td>Equivalent to <a href="#echo-0"><tt>echo()</tt></a>.</td></tr><tr><td valign="top"><a href="#get_columns-3">get_columns/3</a></td><td>Get columns of table.</td></tr><tr><td valign="top"><a href="#get_columns-2">get_columns/2</a></td><td>Equivalent to <a href="#get_columns-3"><tt>get_columns(ovsdb_client, DbName, Table)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_tables-2">get_tables/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_tables-1">get_tables/1</a></td><td>Equivalent to <a href="#get_tables-2"><tt>get_tables(ovsdb_client, DbName)</tt></a>.</td></tr><tr><td valign="top"><a href="#get_content-3">get_content/3</a></td><td></td></tr><tr><td valign="top"><a href="#get_content-2">get_content/2</a></td><td>Equivalent to <a href="#get_content-3"><tt>get_content(ovsdb_client, DbName, Select)</tt></a>.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="start-2"></a>

### start/2 ###

`start(IpPortStr, Opts) -> any()`

<a name="list_dbs-1"></a>

### list_dbs/1 ###

<pre><code>
list_dbs(Dst::<a href="#type-dst">dst()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Lists available databases

Reference [4.1.1. List Databases](https://tools.ietf.org.md/rfc7047#section-4.1.1)

<a name="list_dbs-0"></a>

### list_dbs/0 ###

`list_dbs() -> any()`

Equivalent to [`list_dbs(ovsdb_client)`](#list_dbs-1).

<a name="get_schema-2"></a>

### get_schema/2 ###

<pre><code>
get_schema(Dst::<a href="#type-dst">dst()</a>, DbName::<a href="#type-db_name">db_name()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Get database schema

Reference [4.1.2. Get Schema](https://tools.ietf.org.md/rfc7047#section-4.1.2)

<a name="get_schema-1"></a>

### get_schema/1 ###

`get_schema(DbName) -> any()`

Equivalent to [`get_schema(ovsdb_client, DbName)`](#get_schema-2).

<a name="transaction-3"></a>

### transaction/3 ###

<pre><code>
transaction(Dst::<a href="#type-dst">dst()</a>, DbName::<a href="#type-db_name">db_name()</a>, Operation::<a href="#type-ovsdb_ops">ovsdb_ops()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Perform OVSDB Transaction

Reference [4.1.3. Transaction](https://tools.ietf.org.md/rfc7047#section-4.1.3)

<a name="transaction-2"></a>

### transaction/2 ###

`transaction(DbName, Operation) -> any()`

Equivalent to [`transaction(ovsdb_client, DbName, Operation)`](#transaction-3).

<a name="cancel-3"></a>

### cancel/3 ###

<pre><code>
cancel(Dst::<a href="#type-dst">dst()</a>, DbName::<a href="#type-db_name">db_name()</a>, Operation::<a href="#type-ovsdb_ops">ovsdb_ops()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Cancel Transaction

Reference [4.1.4. Cancel](https://tools.ietf.org.md/rfc7047#section-4.1.4)

<a name="cancel-2"></a>

### cancel/2 ###

`cancel(DbName, Operation) -> any()`

Equivalent to [`cancel(ovsdb_client, DbName, Operation)`](#cancel-3).

<a name="monitor-4"></a>

### monitor/4 ###

<pre><code>
monitor(Dst::<a href="#type-dst">dst()</a>, Id::<a href="#type-json_value">json_value()</a>, DbName::<a href="#type-db_name">db_name()</a>, Select::term()) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Monitor

Caller initiates monitor session identified by an Id. This Id need to be used to cancel/stop
monitor operation. All monitored events will be sent to caller process with following
syntax.

```
     {ovsdb_monitor, Id, Update}
```

Reference [4.1.5. Monitor](https://tools.ietf.org.md/rfc7047#section-4.1.5)

<a name="monitor-3"></a>

### monitor/3 ###

`monitor(Id, DbName, Select) -> any()`

Equivalent to [`monitor(ovsdb_client, Id, DbName, Select)`](#monitor-4).

<a name="monitor_cancel-2"></a>

### monitor_cancel/2 ###

<pre><code>
monitor_cancel(Dst::<a href="#type-dst">dst()</a>, Id::<a href="#type-json_value">json_value()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Cancel Monitor Operation

Reference [4.1.7. Monitor Cancellation](https://tools.ietf.org.md/rfc7047#section-4.1.7)

<a name="monitor_cancel-1"></a>

### monitor_cancel/1 ###

`monitor_cancel(Id) -> any()`

Equivalent to [`monitor_cancel(ovsdb_client, Id)`](#monitor_cancel-2).

<a name="lock-2"></a>

### lock/2 ###

<pre><code>
lock(Dst::<a href="#type-dst">dst()</a>, Id::<a href="#type-json_value">json_value()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Lock Database

This function returns with status of database getting locked or not. If it is not locked,
a notification will be sent when the database is locked. And it would like this:

```
     {ovsdb_notification, Method, LockId}
           Method: locked | stolen
```

Reference [4.1.8. Lock Operations](https://tools.ietf.org.md/rfc7047#section-4.1.8)

<a name="lock-1"></a>

### lock/1 ###

`lock(Id) -> any()`

Equivalent to [`lock(ovsdb_client, Id)`](#lock-2).

<a name="steal-2"></a>

### steal/2 ###

<pre><code>
steal(Dst::<a href="#type-dst">dst()</a>, Id::<a href="#type-json_value">json_value()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Steal lock

This method would forcefully grab the database access which was previously locked
by Id by another process. This operation would notify the process which originally
had lock that the lock is stolen.

Reference [4.1.8. Lock Operations](https://tools.ietf.org.md/rfc7047#section-4.1.8)

<a name="steal-1"></a>

### steal/1 ###

`steal(Id) -> any()`

Equivalent to [`steal(ovsdb_client, Id)`](#steal-2).

<a name="unlock-2"></a>

### unlock/2 ###

<pre><code>
unlock(Dst::<a href="#type-dst">dst()</a>, Id::<a href="#type-json_value">json_value()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Unlock database

Unlock the database by releasing the lock.

Reference [4.1.8. Lock Operations](https://tools.ietf.org.md/rfc7047#section-4.1.8)

<a name="unlock-1"></a>

### unlock/1 ###

`unlock(Id) -> any()`

Equivalent to [`unlock(ovsdb_client, Id)`](#unlock-2).

<a name="echo-1"></a>

### echo/1 ###

<pre><code>
echo(Dst::<a href="#type-dst">dst()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Echo

This can be used to check by caller if the session is active. By default, ovsdb_client
performs echo - echo-reply handshakes to makesure session is active. This function can
be used for debugging.

Reference [4.1.11. Echo](https://tools.ietf.org.md/rfc7047#section-4.1.11)

<a name="echo-0"></a>

### echo/0 ###

`echo() -> any()`

Equivalent to [`echo()`](#echo-0).

<a name="get_columns-3"></a>

### get_columns/3 ###

<pre><code>
get_columns(Dst::<a href="#type-dst">dst()</a>, DbName::<a href="#type-db_name">db_name()</a>, Table::<a href="#type-db_table">db_table()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

Get columns of table

Reference [4.1.3. Transaction](https://tools.ietf.org.md/rfc7047#section-4.1.3)

<a name="get_columns-2"></a>

### get_columns/2 ###

`get_columns(DbName, Table) -> any()`

Equivalent to [`get_columns(ovsdb_client, DbName, Table)`](#get_columns-3).

<a name="get_tables-2"></a>

### get_tables/2 ###

<pre><code>
get_tables(Dst::<a href="#type-dst">dst()</a>, DbName::<a href="#type-db_name">db_name()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

<a name="get_tables-1"></a>

### get_tables/1 ###

`get_tables(DbName) -> any()`

Equivalent to [`get_tables(ovsdb_client, DbName)`](#get_tables-2).

<a name="get_content-3"></a>

### get_content/3 ###

<pre><code>
get_content(Dst::<a href="#type-dst">dst()</a>, DbName::<a href="#type-db_name">db_name()</a>, Select::<a href="#type-select">select()</a>) -&gt; <a href="#type-rpc_return">rpc_return()</a>
</code></pre>
<br />

<a name="get_content-2"></a>

### get_content/2 ###

`get_content(DbName, Select) -> any()`

Equivalent to [`get_content(ovsdb_client, DbName, Select)`](#get_content-3).

