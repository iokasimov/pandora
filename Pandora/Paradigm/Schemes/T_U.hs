{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Schemes.T_U where

import Pandora.Core.Functor (type (>))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Morphism.Flip (Flip)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|-|-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>-|-|-)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (=#-), (-#=)))

newtype T_U ct cu p t u a = T_U (p (t a) (u a))

infixr 2 <:.:>, >:.:>, <:.:<, >:.:<

type (<:.:>) t u p = T_U Covariant Covariant p t u
type (>:.:>) t u p = T_U Contravariant Covariant p t u
type (<:.:<) t u p = T_U Covariant Contravariant p t u
type (>:.:<) t u p = T_U Contravariant Contravariant p t u

instance Interpreted (->) (T_U ct cu p t u) where
	type Primary (T_U ct cu p t u) a = p (t a) (u a)
	run ~(T_U x) = x
	unite = T_U

-- TODO: generalize over (->)
instance (forall i . Covariant (->) (->) (p i), forall o . Covariant (->) (->) (Flip p o), Covariant (->) (->) t, Covariant (->) (->) u) => Covariant (->) (->) (t <:.:> u > p) where
	f <-|- x = ((-#=) @_ @(Flip _ _) ((<-|-|-) f) . ((<-|-|-) f)) =#- x

-- TODO: generalize over (->)
instance (Contravariant (->) (->) t, forall a . Covariant (->) (->) (p (t a)), Covariant (->) (->) u, forall b . Contravariant (->) (->) (Flip p (u b))) => Covariant (->) (->) (t >:.:> u > p) where
	(<-|-) f = (=#-) ((-#=) @_ @(Flip _ _) ((>-|-|-) f) . ((<-|-|-) f))
