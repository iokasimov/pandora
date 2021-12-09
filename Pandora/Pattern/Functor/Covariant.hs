{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Pattern.Functor.Covariant where

import Pandora.Pattern.Betwixt (Betwixt)
import Pandora.Pattern.Semigroupoid (Semigroupoid)

infixl 4 <-|-, <$>
infixl 3 <-|-|-, <$$>
infixl 4 <-|-|-|-, <$$$>

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

(<$$$$>) :: forall source between1 between2 between3 target t u v w a b
	. (Covariant source between1 w, Covariant between1 between2 v, Covariant between2 between3 u, Covariant between3 target t)
	=> source a b -> target (t (u (v (w a)))) (t (u (v (w b))))
(<$$$$>) s = ((<-|-) @between3 @target @t ((<-|-) @between2 @between3 @u ((<-|-) @between1 @between2 @v ((<-|-) @source @between1 @w s))))
