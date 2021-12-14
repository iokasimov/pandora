{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Pattern.Functor.Covariant where

import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid)

infixl 4 <-|-, <$>
infixl 3 <-|-|-, <$$>
infixl 2 <-|-|-|-, <$$$>

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity <-|-) ≡ identity
> * Interpreted of morphisms: (f . g <-|-) ≡ (f <-|-) . (g <-|-)
-}

class (Semigroupoid source, Semigroupoid target) => Covariant source target t where
	(<-|-) :: source a b -> target (t a) (t b)

(<-|-|-) :: forall source target t u a b
	. (Covariant source (Betwixt source target) u, Covariant (Betwixt source target) target t)
	=> source a b -> target (t (u a)) (t (u b))
(<-|-|-) s = ((<-|-) ((<-|-) @source @(Betwixt source target) @u s))

(<-|-|-|-) :: forall source target t u v a b
	. (Covariant source (Betwixt source (Betwixt source target)) v, Covariant (Betwixt source (Betwixt source target)) (Betwixt (Betwixt source target) target) u, Covariant (Betwixt (Betwixt source target) target) target t)
	=> source a b -> target (t (u (v a))) (t (u (v b)))
(<-|-|-|-) s = ((<-|-) @(Betwixt (Betwixt source target) target) @target ((<-|-) @(Betwixt source (Betwixt source target)) @(Betwixt (Betwixt source target) target) @u ((<-|-) @source @(Betwixt source (Betwixt source target)) @v s)))

(<$>) :: Covariant source target t => source a b -> target (t a) (t b)
(<$>) = (<-|-)

(<$$>) :: (Covariant source (Betwixt source target) u, Covariant (Betwixt source target) target t) => source a b -> target (t (u a)) (t (u b))
(<$$>) = (<-|-|-)

(<$$$>) :: forall source target t u v a b
	. (Covariant source (Betwixt source (Betwixt source target)) v, Covariant (Betwixt source (Betwixt source target)) (Betwixt (Betwixt source target) target) u, Covariant (Betwixt (Betwixt source target) target) target t)
	=> source a b -> target (t (u (v a))) (t (u (v b)))
(<$$$>) s = (<-|-|-|-) s
