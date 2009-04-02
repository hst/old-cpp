% CSP₀ file format
% Douglas Creager <dcreager@alum.mit.edu>
% February 2009

# Introduction

The CSP₀ language can be seen as an “assembly language” for the CSP
process algebra.  It does not contain any of the high-level functional
programming constructs that CSP~M~ has.  Instead, it's intended that
higher-level, more “useful” CSP languages (such as CSP~M~) will be
compiled into CSP₀.

# Recursion

There is limited support for recursion in CSP₀, via _finalized_
processes.  A process must be declared with a `process` statement, but
the process isn't finalized until after it has been defined with an
operator statement.  Many process parameters require a finalized
process, which don't allow recursion.  However, some process
parameters work with processes that have been declared but not yet
defined.  The most notable of these parameters is the `prefix`
statement; the underlying rule is that a process parameter only needs
to be finalized we need to know its initials and afters set in order
to define the operator's semantics.

# Syntax reference

## Identifiers

Identifiers must begin with a letter, underscore, or dollar sign.  The
remaining characters must be letters, numbers, periods, or
underscores.  Those that begin with a dollar sign must contain at
least one more character — ‘`$`’ on its own is not a valid identifier.

The intention is that higher-level languages shouldn't allow
identifiers beginning with `$`; instead, these identifiers are
available for the compiler that translates the higher-level language
into CSP₀ in case it needs to create any temporary processes as part
of the compilation process.

## Declarations

    event ‹id›;

Declares a new event.  An event must be declared before it can be used
in any process definition.  It is illegal to declare an event more
than once.

    process ‹id›;

Declares a new process.  A process must be declared before it can be
defined with an operator statement.  It is illegal to declare a
process more than once.

## Predefined events and processes

There are two predefined events (τ and ✓) and predefined processes
(`STOP` and `SKIP`).  The `STOP` process cannot perform any events
whatsover.  The `SKIP` process can perform a ✓, after which it behaves
like `STOP`.  The events are not directly accessible in a CSP₀ script.
τ events are created automatically by certain CSP operators, while ✓
is only available via the `SKIP` process.  Just like any other
process, it's illegal to try to redefine `STOP` or `SKIP`.

## Alphabets

    { ‹e1›, ‹e2›... }

An alphabet is a set of events (possibly empty), separated by commas,
and surrounded by curly braces.  It isn't illegal for an event to
appear in the alphabet more than once, but this has no effect.

## Process sets

    { ‹P1›, ‹P2›... }

A process set is a set of process names (possibly empty), separated by
commas, and surrounded by curly braces.  It isn't illegal for a
process to appear in the set more than once, but this has no effect.

## Event maps

    [[ ‹d1› -> ‹r1›, ‹d2› -> ‹r2›... ]]

An event map is a set of event pairs.  The events in each pair are
separated by a right-arrow.  The pairs in the list are separated by
commas.  The overall map is surrounded by double square brackets.  As
with alphabets, an event pair is allowed to appear in the map more
than once, but this has no effect.

## Operator statements

For detailed descriptions of the CSP operators, please see
[[1]](#bib1).

    prefix ‹P› = ‹e› -> ‹Q›;

Creates a prefix process.  _‹P›_ can perform an _‹e›_ event, after
which it behaves like _‹Q›_.  _‹P›_ must not be finalized.  _‹Q›_ can
either be finalized or not.

    extchoice ‹P› = ‹Q› [] ‹R›;

Creates an external choice process.  _‹P›_ presents the environment
with the choice of acting like _‹Q›_ or _‹R›_.  _‹P›_ must not be
finalized.  _‹Q›_ and _‹R›_ must both be finalized.

    intchoice ‹P› = ‹Q› |~| ‹R›;

Creates an internal choice process.  _‹P›_ nondeterministically
chooses whether to behave like _‹Q›_ or _‹R›_.  At least one of the
subprocesses will be available, but the other might be refused.  _‹P›_
must not be finalized.  _‹Q›_ and _‹R›_ can either be finalized or
not.

    timeout ‹P› = ‹Q› [> ‹R›;

Creates a timeout process.  _‹P›_ behaves like _‹Q›_ initially, but at
any point in the future (nondeterministically), it can start to refuse
_‹Q›_ and behave like _‹R›_ instead.  _‹P›_ must not be finalized.
_‹Q›_ must be finalized.  _‹R›_ can either be finalized or not.

    seqcomp ‹P› = ‹Q› ; ‹R›;

Creates a sequential composition process.  _‹P›_ behaves like _‹Q›_
until a ✓ occurs (signaling termination), after which it behaves like
_‹R›_.  _‹P›_ must not be finalized.  _‹Q›_ must be finalized.  _‹R›_
can either be finalized or not.

    interleave ‹P› = ‹Q› ||| ‹R›;

Creates an interleave process.  _‹P›_ behaves like _‹Q›_ and _‹R›_
simultaneously.  The events of _‹Q›_ and _‹R›_ can occur in any order.
_‹P›_ cannot terminate until both _‹Q›_ and _‹R›_ have terminated.
_‹P›_ must not be finalized.  _‹Q›_ and _‹R›_ must both be finalized.

    aparallel ‹P› = ‹Q› [| ‹α› |] ‹R›;

Creates an alphabetized parallel process.  _‹P›_ behaves like _‹Q›_
and _‹R›_ simultaneously; however, for all of the events in _‹α›_,
_‹Q›_ and _‹R›_ must both be able to perform the event for _‹P›_ to be
able to perform it.  _‹P›_ cannot terminate until both _‹Q›_ and _‹R›_
have terminated.  _‹P›_ must not be finalized.  _‹Q›_ and _‹R›_ must
both be finalized.

    iparallel ‹P› = ‹Q› [ ‹αQ› || ‹αR› ] ‹R›;

Creates an interface parallel process.  _‹P›_ behaves like _‹Q›_ and
_‹R›_ simultaneously; however, for all of the events in the
intersection of _‹αQ›_ and _‹αR›_, _‹Q›_ and _‹R›_ must both be able
to perform the event for _‹P›_ to be able to perform it.  _‹P›_ cannot
terminate until both _‹Q›_ and _‹R›_ have terminated.  _‹P›_ must not
be finalized.  _‹Q›_ and _‹R›_ must both be finalized.

    hide ‹P› = ‹Q› \ ‹α›;

Creates a hiding process.  _‹P›_ behaves like _‹Q›_, but with all of
the events in _‹α›_ hidden, and replaced with τs (which are invisible
to the environment).  _‹P›_ must not be finalized.  _‹Q›_ must be
finalized.

    rename ‹P› = ‹Q› [[ ‹μ› ]];

Creates a renaming process.  _‹P›_ behaves like _‹Q›_, but with all of
the events renamed occording to _‹μ›_.  If one of _‹Q›_'s events
appears in the map's domain, it is replaced with the events that it is
mapped to.  _‹P›_ must not be finalized.  _‹Q›_ must be finalized.

    rextchoice ‹P› = [] { ‹φ› };

Creates a replicated external choice process.  _‹P›_ presents the
environment with the choice of acting like any of the processes in
_‹φ›_.  _‹P›_ must not be finalized.  Each process in _‹φ›_ must be
finalized.

    rintchoice ‹P› = |~| { ‹φ› };

Creates a replicated internal choice process.  _‹P›_ will
nondeterministically choose to act like any of the processes in _‹φ›_.
_‹P›_ must not be finalized.  Each process in _‹φ›_ can either be
finalized or not.  The set _‹φ›_ must not be empty.


# References

[1] <a name="bib1"/>
:   A. W. Roscoe.  The theory and practice of concurrency.  Prentice
    Hall, 1998.  ISBN 0-13-6774409-5.
    <http://web.comlab.ox.ac.uk/oucl/publications/books/concurrency/>
