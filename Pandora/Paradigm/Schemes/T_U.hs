module Pandora.Paradigm.Schemes.T_U where

import Pandora.Pattern.Functor.Covariant (Covariant ((<$>)))
import Pandora.Pattern.Functor.Contravariant (Contravariant ((>$<)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)))

newtype T_U ct cu t p u a = T_U (p (t a) (u a))

type (<:.:>) p t = T_U Covariant Covariant t p t
type (>:.:>) p t = T_U Contravariant Covariant t p t
type (<:.:<) p t = T_U Covariant Contravariant t p t
type (>:.:<) p t = T_U Contravariant Contravariant t p t

instance Interpreted (T_U ct cu t p u) where
	type Primary (T_U ct cu t p u) a = p (t a) (u a)
	run ~(T_U x) = x
	unite = T_U

instance (Bivariant p, Covariant t, Covariant u)
	=> Covariant (T_U Covariant Covariant t p u) where
		f <$> x = ((f <$>) <-> (f <$>)) ||= x

instance (Divariant p, Contravariant t, Covariant u)
	=> Covariant (T_U Contravariant Covariant t p u) where
		f <$> x = ((f >$<) >-> (f <$>)) ||= x
