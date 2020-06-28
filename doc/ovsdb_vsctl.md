

# Module ovsdb_vsctl #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

Module providing APIs in lines similar to that of ovs-vsctl utility.

__Authors:__ Vasu Dasari.

<a name="types"></a>

## Data Types ##




### <a name="type-vsctl_returns">vsctl_returns()</a> ###


<pre><code>
vsctl_returns() = ok | error | {ok, term()} | {error, term()}
</code></pre>

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_br-2">add_br/2</a></td><td>Add/Modify a bridge to switch.</td></tr><tr><td valign="top"><a href="#del_br-2">del_br/2</a></td><td>Deletes a bridge to switch.</td></tr><tr><td valign="top"><a href="#add_port-3">add_port/3</a></td><td>Add/modify port to a bridge.</td></tr><tr><td valign="top"><a href="#del_port-3">del_port/3</a></td><td>Add/modify port to a bridge.</td></tr><tr><td valign="top"><a href="#add_bond-4">add_bond/4</a></td><td>Create or modify bond interface.</td></tr><tr><td valign="top"><a href="#add_bond_iface-4">add_bond_iface/4</a></td><td></td></tr><tr><td valign="top"><a href="#del_bond_iface-4">del_bond_iface/4</a></td><td></td></tr><tr><td valign="top"><a href="#del_bond-3">del_bond/3</a></td><td></td></tr><tr><td valign="top"><a href="#trace-1">trace/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_br-2"></a>

### add_br/2 ###

<pre><code>
add_br(BrName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, Opts::<a href="ovsdb_client.md#type-opts">ovsdb_client:opts()</a>) -&gt; <a href="#type-vsctl_returns">vsctl_returns()</a>
</code></pre>
<br />

Add/Modify a bridge to switch

<a name="del_br-2"></a>

### del_br/2 ###

<pre><code>
del_br(BrName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, Opts::<a href="ovsdb_client.md#type-opts">ovsdb_client:opts()</a>) -&gt; <a href="#type-vsctl_returns">vsctl_returns()</a>
</code></pre>
<br />

Deletes a bridge to switch

<a name="add_port-3"></a>

### add_port/3 ###

<pre><code>
add_port(BrName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, PortName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, Opts::<a href="ovsdb_client.md#type-opts">ovsdb_client:opts()</a>) -&gt; <a href="#type-vsctl_returns">vsctl_returns()</a>
</code></pre>
<br />

Add/modify port to a bridge

Bridge of type netdev will be created if it does not exists

<a name="del_port-3"></a>

### del_port/3 ###

<pre><code>
del_port(BrName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, PortName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, Opts::<a href="ovsdb_client.md#type-opts">ovsdb_client:opts()</a>) -&gt; <a href="#type-vsctl_returns">vsctl_returns()</a>
</code></pre>
<br />

Add/modify port to a bridge

<a name="add_bond-4"></a>

### add_bond/4 ###

<pre><code>
add_bond(BrName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, BondName::<a href="unicode.md#type-chardata">unicode:chardata()</a>, IfaceList::list(), Opts::<a href="ovsdb_client.md#type-opts">ovsdb_client:opts()</a>) -&gt; <a href="ovsdb_client.md#type-rpc_return">ovsdb_client:rpc_return()</a>
</code></pre>
<br />

Create or modify bond interface

<a name="add_bond_iface-4"></a>

### add_bond_iface/4 ###

`add_bond_iface(BrName, BondName, Iface, Opts) -> any()`

<a name="del_bond_iface-4"></a>

### del_bond_iface/4 ###

`del_bond_iface(BrName, BondName, Iface, Opts) -> any()`

<a name="del_bond-3"></a>

### del_bond/3 ###

`del_bond(BrName, BondName, Opts) -> any()`

<a name="trace-1"></a>

### trace/1 ###

`trace(X1) -> any()`

