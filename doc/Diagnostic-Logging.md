# The Hierarchical Log Library (`hlog`)

A program uses the hierarchical log library, `hlog`, to organize
its diagnostic messages into categories and subcategories and to
turn on and off message categories to produce the most useful
diagnostic trace.

A typical program will define one or more log *outlets*.  An outlet
is a named target for diagnostic messages.  Each outlet has a *state*
(*on*, *off*, or *pass*) and at most one *parent outlet*.  Usually, the
parent-child relationships between outlets form a tree rooted at the
outlet "all", which is supplied by the library.  Outlets may form a
"forest" if a program supplies its own root outlets.

A program sends messages to an outlet using `hlog` API calls.
Messages sent to an outlet that is *on* are copied to the error
stream.  Messages sent to an outlet that is *off* are discarded.
When a message is sent to an outlet in *pass* state, the `hlog`
uses the outlet ancestors to decide what to do with the message.

## Sending messages with `hlog_fast`
 
A program calls `hlog_fast(outlet name, format string, ...)` to
write a formatted message to the named outlet.  `hlog_fast` uses
the outlet state to do decide what to do with the message.  If the
outlet is *on*, then the message is written to the standard error
stream.  If the outlet is *off*, then the message is discarded.
If the outlet is in state *pass*, and the outlet has no parent,
then the message is discarded.  If the outlet does have a parent,
then `hlog_fast` looks at the parent state and decides whether to
discard, write, or recurse.

`hlog_fast` precedes each diagnostic message with a timestamp (decimal
seconds with 9 digits right of the decimal point), a colon, and a single
space.  Each message is followed with a newline ("\n").  The effective
timestamp resolution may be much less than one nanosecond.  Timestamps
increase monotonically.  The timestamp origin is currently unspecified.

## Defining log outlets

`hlog` provides macros for declaring outlets, and for statically
configuring an outlet, its parent, and its initial state.

Use `HLOG_OUTLET_DECL(name)` to declare shared outlets in header
files.  `HLOG_OUTLET_DECL(name)` declares an `extern` symbol.
There must not be any quotation marks on *name*.

Use `HLOG_OUTLET_SHORT_DEFN(name, parent)` to define an outlet with the
given name and parent in state *pass*.  There must not be any quotation
marks on *name*.

Use `HLOG_OUTLET_MEDIUM_DEFN(name, parent, state)` to define an outlet
with the given name, parent, and state.  The state is given by an
`hlog_outlet_state_t`, one of `HLOG_OUTLET_S_ON`, `HLOG_OUTLET_S_OFF`,
or `HLOG_OUTLET_S_PASS`.  There must not be any quotation marks on
*name*.

## Enabling and disabling outlets with the environment

An environment variable, `HLOG`, sets initial outlet states for a
program.  If `HLOG` may be set to the empty string, in which case
outlet states stay at their program defaults.  `HLOG` may also be
set to one or more *outlet name*=*state* pairs, separated by either
whitespace or commas. *state* is one of *pass*, *on*, or *off*,
and *outlet-name* is a string matching `[_a-zA-Z][_a-zA-Z0-9]*`.
For example, to enable the `tick` outlet and `pbrm` outlets while
the program `./vfd_swmr_zoo_writer` runs, you can use this command
in `csh` or Bourne shell:

```
env HLOG="tick=on pbrm=on" ./vfd_swmr_zoo_writer
```

# Implementation notes

`hlog_fast(outlet name, format string, ...)` is implemented as a
macro that only evaluates its format string or other arguments if
it decides to write the message to `stderr`.  `hlog_fast` avoids
repeatedly walking child-parent links by caching its decision to
write or discard in the named outlet.

# Future improvements

The timestamp origin is unspecified, now.  For the user's convenience,
the timestamp probably should be measured from `hlog` library
initialization.  Also, `hlog` should provide a routine for setting
the timestamp origin to the current time.

