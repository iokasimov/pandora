# Architecture

This document describes the high-level architecture of rust-analyzer.
If you want to familiarize yourself with the code base, you are just in the right place!

# Code base

There are two important sections:

* `Pattern`: contains mostly typeclasses and their laws that you should obey before adding new instance. Names are taken from category theory, abstract algebra and other Haskell packages.

* `Paradigm`: control flow primitives and data structures that you can use.
