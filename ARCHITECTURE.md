Architecture

This document describes the high-level architecture of Pandora library.
If you want to familiarize yourself with the code base, you are just in the right place!

# Code base

There are two important sections:

* `Pattern`: contains mostly typeclasses and their laws that you should obey before adding a new instance. Names are taken from category theory, abstract algebra and other Haskell packages.

* `Paradigm`: control flow primitives and data structures that you can use.

Imports in all modules are not qualified (it prevents name clashes) and provided with exhaustive list of identifiers.

# Implementation details

## Functors, not endofunctors

All functors definition include information about source and target category, for example:

```
Covariant source target t
Contravariant source target t
```

## Right-to-left operators

Due to generalized definition, many operators consuming arguments from right to left:

```
<-|-, <-*-, =<<, <<-, -+-
```
