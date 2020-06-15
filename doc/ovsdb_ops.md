

# Module ovsdb_ops #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

OVSDB Database manipulation methods.

__Authors:__ Vasu Dasari.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#insert-2">insert/2</a></td><td></td></tr><tr><td valign="top"><a href="#insert-3">insert/3</a></td><td></td></tr><tr><td valign="top"><a href="#select-3">select/3</a></td><td></td></tr><tr><td valign="top"><a href="#update-3">update/3</a></td><td></td></tr><tr><td valign="top"><a href="#mutate-3">mutate/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-2">delete/2</a></td><td></td></tr><tr><td valign="top"><a href="#wait-4">wait/4</a></td><td></td></tr><tr><td valign="top"><a href="#wait-5">wait/5</a></td><td></td></tr><tr><td valign="top"><a href="#commit-1">commit/1</a></td><td></td></tr><tr><td valign="top"><a href="#abort-0">abort/0</a></td><td></td></tr><tr><td valign="top"><a href="#comment-1">comment/1</a></td><td></td></tr><tr><td valign="top"><a href="#assert-1">assert/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="insert-2"></a>

### insert/2 ###

`insert(Table, Row) -> any()`

<a name="insert-3"></a>

### insert/3 ###

`insert(Table, Row, Id) -> any()`

<a name="select-3"></a>

### select/3 ###

`select(Columns, Table, Conditions) -> any()`

<a name="update-3"></a>

### update/3 ###

`update(Table, Conditions, Row) -> any()`

<a name="mutate-3"></a>

### mutate/3 ###

`mutate(Table, Conditions, Mutations) -> any()`

<a name="delete-2"></a>

### delete/2 ###

`delete(Table, Conditions) -> any()`

<a name="wait-4"></a>

### wait/4 ###

`wait(Table, Conditions, Rows, Until) -> any()`

<a name="wait-5"></a>

### wait/5 ###

`wait(Table, Conditions, Rows, Until, Columns) -> any()`

<a name="commit-1"></a>

### commit/1 ###

`commit(Mode) -> any()`

<a name="abort-0"></a>

### abort/0 ###

`abort() -> any()`

<a name="comment-1"></a>

### comment/1 ###

`comment(Comment) -> any()`

<a name="assert-1"></a>

### assert/1 ###

`assert(Lock) -> any()`

