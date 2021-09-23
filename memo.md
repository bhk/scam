# memo.scm

The `memo` module implements persistent memoization.  Memoization caches the
results of function calls to avoid re-computing them.  "Persistent" means
that results are available not just during the invocation of a program, but
during future invocations as well.  Also, persistent memoization can be
applied to functions that perform IO.  SCAM uses persistent memoization to
rebuild SCAM programs accurately and efficiently.  [The `memo` module
functionality is not to be confused with the `memoize` function in the
`core` library.]

The `memo.scm` source file contains API documentation.  This document
describes the theory of operation, and introduces some terminology,
concepts, and implications that programmers should be aware of when using
this module.

## Wrapping

In order to utilize memoization, the programmer must identify what
procedures are to be memoized and "wrap" all calls to them.  For example, to
wrap the following function call:

    (foo a b c)

... we replace it with:

    (memo-call (native-name foo) a b c)

We say that a function call is "memoized" when calls to it are wrapped.

Memoization of a function call is *valid* when the memoized form is
*equivalent* to the original form.  By "equivalent" we mean that the
behavior of the program will not be affected by this transformation
(ignoring, of course, time of execution and writes to cache files done by
the memoization module itself).

For any SCAM function that is a "pure function" -- that is, its result value
is uniquely determined by its inputs, and it has no side effects --
supplying a cached value from previous invocation (with the same arguments)
will be indistinguishable from invoking the procedure again.  Therefore,
memoization of any pure function will always be valid.


## IO

IO operations have two different impacts on memoization.  First, the return
value of each IO operation constitutes an additional "input" to the memoized
procedure, since it can affect the subsequent behavior of the procedure.
The memoization cannot use a cached return value (and *skip* calling the
actual function) until it validates that the previously-called IO operations
will return the same values (if they were to be called).

Second, an IO operation can have side effects.  In order to ensure that
memoization is valid -- i.e., that a memoized call is equivalent to actual
invocation of the function -- the memoized call will skip invocation only
when it can validate that side effects are equivalent (or make it so).

When a memoized function is invoked, its IO operations are logged, and
stored in the memoization cache along with the function name, arguments, and
return value.  In order to enable this, *memoized functions must apprise the
memo module of all IO they perform*.  They can do this by using the IO
functions exposed by the memo module -- memo-read-file, memo-write-file,
etc. -- or by constructing custom IO functions and calling them through
`memo-io`.

To ensure validity of memoization with IO, additional constraints apply to
the memoized functions.  To describe them, we first need to introduce the
concept of a **session**.

A session is a period of time during which we make assumptions of
immutability of external files.  Specifically: during a session, no changes
will be made to a file that will invalidate a prior read or write made by a
memoized function.  *Between* sessions, we assume that files and other
external state may change, but *during* a session we assume that files
written to or read by your memoized functions are not being modified by some
external process.

A program uses the `memo-on` function to control the scope of sessions.

To summarize the constraints:

 - No write-after-read: During a session, your program may not write
   to a file that has been read by a memoized function.

 - No write-after-write: During a session, your program may not write
   to a file that has been written to by a memoized function.


## Nested Calls

Memoized procedures can call other memoized procedures.  Values returned by
a nested call are treated as dependencies of the parent, similarly to how IO
operations are handled.  Otherwise, each memoized operation is cached
independently.

For example, consider two memoized functions: f and g, and a previously
cached call in which f called g, and in which both f and g perform IO.  It
is possible for g to experience a cache miss (requiring re-invoking the
original procedure) without f experiencing a cache miss.  For example, a
file read by g might have changed, which will cause g to be re-invoked, but
as long as the return value of g remains the same it will not invalidate the
cached result for f.  Likewise, changes to f's inputs or f's IO results
could cause a cache miss for f while g's cache entries remain relevant and
usable.


## More on Sessions

When a memoized function is called with a set of arguments and no match is
found in the cache DB, we invoke the function and log all the IO operations
it performs in the cache DB along with the function name, its arguments, and
its result.

When a subsequent matching call (in which the function name and arguments
are all the same) is made, what actually happens depends on whether this
new call takes place during the same session as the logged call.

 1. If this new call occurs within the same session as the cache entry, we
    simply return the cached result.  There is no need to re-validate the IO
    reads or writes, due to immutability assumptions that hold during a
    session.  We call this an intra-session cache hit.

 2. If the new call occurs in a later, *different* session, we cannot assume
    that file inputs and outputs have not changed.  Therefore, we proceed to
    *replay* the logged IO operations to validate that the input files have
    not changed, and to ensure that written files are as they were, or make
    them so.  If all the replayed operations succeed, then the cached result
    is returned.  We call this a cross-session cache hit.  On the other hand,
    if one of the replayed operations returns a different result than what
    was logged, we abandon the replay attempt and fall back to invoking the
    function, again logging its IO operations and its result in the cache.
    Either way, whether via replay validation or invocation, the result will
    be available for subsequent calls within the same session without
    further replay.

The assumption that external processes will not modify files is nothing new.
For example, compilers read source files and write out object files.  In
order to make any assertions about the behavior of the program, we *must*
assume that, while it is compiling, no external processes are modifying
those files -- otherwise, there would be no way to predict its behavior.

We extend these immutability assumptions to cover the program itself so that
we can implement intra-session memoization.  As in classic memoization, an
intra-session cache hit returns a value immediately.  This can change time
complexity of a program from exponential to linear.

Inter-session cache hits provide a very different benefit.  They allow
re-use of output files generated by a previous session.  Cross-session
memoization, however, must replay IO to validate file contents, and this
extends to nested memoized functions.  So while we it allows us to avoid
costly computation, cross-session memoization will not change the big-O
order of time complexity.

Consider the SCAM compiler's use of memoization.  Cross-session memoization
without intra-session memoization (enabled by the intra-session immutability
assumptions) would yield abysmal performance, due to the potential
exponential time complexity of compilation (each `require` statement in a
module triggers a compilation).  On the other hand, intra-session
memoization without cross-session memoization would leave us without
incremental build capability.


## Proxy IO Operations

There are a couple of important optimizations that keep the cache DB size
manageable.  Since `read-file` may have very large return values, and
`write-file` may have very large input values, treating them as IO
dependencies in the most straightforward manner (as described above) would
result in a very large memoization database.

When `memo-read-file` reads the contents of a file, the IO operation that is
logged (and later replayed) is `do-hash-file` instead of `read-file`.
Hashing serves as a proxy for reading: any change to the file that would
invalidate the read result would also invalidate the hash result.

When `memo-write-file` writes data to a file, it writes the data to a
content-addressed BLOB store then uses `do-write-blob` to copy the BLOB to
the destination file.  Creating the BLOB is not logged, only `do-write-blob`
is.  If, for some reason, the BLOB has disappeared from the cache,
`do-write-blob` will fail and we will fall back to invoking the wrapped
function.


## Dropping Memoization

A memoized function may choose to "drop" memoization for the current call.
This will prevent cross-session caching of the current invocation,
discarding the cache data relevant to it.  This can be used in error
scenarios in order to avoid cluttering the database with entries that are
unlikely to be valuable.


## Cache Files, Instances, and Conflicts

Persistent caching makes use of a database file, whose name is passed to
`memo-on`.  In addition to the database file, BLOB files are also written
into the directory that contains the database file.

In order for one session to take advantage of cached results from a previous
session, both sessions will have to name the same database file.

Generally speaking, different programs should use different database files.
The database uses function names to identify memoized operations, and two
different programs might assign different definitions to the same name
(especially when the two programs are different versions of the same
project).  Different database files can co-exist in the same directory.  The
SCAM compiler hashes its own source files to generate a unique ID that it
incorporates into its DB name, so that one version of SCAM will not corrupt
the cache of another.  (The BLOB files are written atomically, so one BLOB
cache can be shared.)

If global state differs between two invocations of the same program --
e.g. current directory, or environment variables -- persistent memoization
results might be invalid unless care is taken to avoid this problem.  One
valid approach to this is to use `memo-io` to make memoization aware of this
external variable.  A more efficient approach is to incorporate the external
context information into the database file name (this will result in
multiple independent database files, instead of one large one containing
multiple independent histories).  The SCAM compiler hashes the current
working directory, together with its own source file hash, to obtain its
database file name.

If two instances of a program run concurrently -- with the same memo
database file -- the last one to complete will overwrite the database
results of the other.  This will not cause invalid results; it will just
mean that work done by one of those instances will not be persistently
cached.
