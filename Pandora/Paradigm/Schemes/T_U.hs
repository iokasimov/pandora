module Pandora.Paradigm.Schemes.T_U where

import Pandora.Core.Functor (type (:=))
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Contravariant (Contravariant, Contravariant ((->$<-)))
import Pandora.Pattern.Functor.Bivariant (Bivariant ((<->)))
import Pandora.Pattern.Functor.Divariant (Divariant ((>->)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (||=)))

newtype T_U ct cu p t u a = T_U (p (t a) (u a))

infixr 2 <:.:>, >:.:>, <:.:<, >:.:<

type (<:.:>) t u p = T_U Covariant Covariant p t u
type (>:.:>) t u p = T_U Contravariant Covariant p t u
type (<:.:<) t u p = T_U Covariant Contravariant p t u
type (>:.:<) t u p = T_U Contravariant Contravariant p t u

instance Interpreted (T_U ct cu p t u) where
	type Primary (T_U ct cu p t u) a = p (t a) (u a)
	run ~(T_U x) = x
	unite = T_U

instance (forall i . Covariant (->) (->) (p i), Bivariant (->) (->) (->) p,  Covariant (->) (->) t, Covariant (->) (->) u) => Covariant (->) (->) (t <:.:> u := p) where
	f -<$>- x = (f -<$>-) <-> (f -<$>-) ||= x

instance (Divariant p (->) (->) (->), Contravariant (->) (->) t, Covariant (->) (->) u) => Covariant (->) (->) (t >:.:> u := p) where
	f -<$>- x = (f ->$<-) >-> (f -<$>-) ||= x

instance (forall i . Covariant (->) (->) (p i), Bivariant (->) (->) (->) p, Contravariant (->) (->) t, Contravariant (->) (->) u) => Contravariant (->) (->) (t >:.:< u := p) where
	f ->$<- x = (f ->$<-) <-> (f ->$<-) ||= x
