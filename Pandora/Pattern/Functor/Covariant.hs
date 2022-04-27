{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Pattern.Functor.Covariant where

import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid)

infixl 1 <-|------, <-|-|----
infixl 2 <-|-----, <-|-|-|-, <-|-|---
infixl 3 <-|----, <-|-|--
infixl 4 <-|---, <-|-|-
infixl 5 <-|--
infixl 6 <-|-

{- |
> When providing a new instance, you should ensure it satisfies:
> * Exactly morphism: (identity <-|-) ≡ identity
> * Interpreted of morphisms: (f . g <-|-) ≡ (f <-|-) . (g <-|-)
-}

class (Semigroupoid source, Semigroupoid target) => Covariant source target t where
	(<-|-) :: source a b -> target (t a) (t b)

	(<-|--), (<-|---), (<-|----), (<-|-----), (<-|------),
		(<-|-------), (<-|--------) :: source a b -> target (t a) (t b)
	(<-|--) = (<-|-)
	(<-|---) = (<-|-)
	(<-|----) = (<-|-)
	(<-|-----) = (<-|-)
	(<-|------) = (<-|-)
	(<-|-------) = (<-|-)
	(<-|--------) = (<-|-)

	(<-|-|-), (<-|-|--), (<-|-|---), (<-|-|----), (<-|-|-----), (<-|-|------), (<-|-|-------) :: (Covariant source (Betwixt source target) u, Covariant (Betwixt source target) target t)
		=> source a b -> target (t (u a)) (t (u b))
	(<-|-|-------) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))
	(<-|-|------) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))
	(<-|-|-----) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))
	(<-|-|----) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))
	(<-|-|---) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))
	(<-|-|--) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))
	(<-|-|-) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @_ s))

	(<-|-|-|-) :: (Covariant source (Betwixt source (Betwixt source target)) v, Covariant (Betwixt source (Betwixt source target)) (Betwixt (Betwixt source target) target) u, Covariant (Betwixt (Betwixt source target) target) target t)
		=> source a b -> target (t (u (v a))) (t (u (v b)))
	(<-|-|-|-) s = ((<-|-) @(Betwixt (Betwixt source target) target) @target ((<-|-) @(Betwixt source (Betwixt source target)) @(Betwixt (Betwixt source target) target) @_ ((<-|-) @source @(Betwixt source (Betwixt source target)) @_ s)))

	-- TODO: create <-|-|-|-|-

(<$>) :: Covariant source target t => source a b -> target (t a) (t b)
(<$>) = (<-|-)

(<$$>) :: (Covariant source target t, Covariant source (Betwixt source target) u, Covariant (Betwixt source target) target t) => source a b -> target (t (u a)) (t (u b))
(<$$>) = (<-|-|-)

(<$$$>) :: (Covariant source target t, Covariant source (Betwixt source (Betwixt source target)) v, Covariant (Betwixt source (Betwixt source target)) (Betwixt (Betwixt source target) target) u, Covariant (Betwixt (Betwixt source target) target) target t)
	=> source a b -> target (t (u (v a))) (t (u (v b)))
(<$$$>) s = (<-|-|-|-) s
