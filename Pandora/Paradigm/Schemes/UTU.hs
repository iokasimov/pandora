module Pandora.Paradigm.Schemes.UTU where

import Pandora.Core.Functor (type (:.), type (>>>))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Contravariant (Contravariant)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite))

newtype UTU ct cu t u u' a = UTU (u :. t :. u' >>> a)

type (<.<:>.>) = UTU Covariant Covariant Covariant
type (>.<:>.>) = UTU Contravariant Covariant Covariant
type (<.<:>.<) = UTU Covariant Covariant Contravariant
type (>.<:>.<) = UTU Contravariant Covariant Contravariant
type (<.>:<.>) = UTU Covariant Contravariant Covariant
type (>.>:<.>) = UTU Contravariant Contravariant Covariant
type (<.>:<.<) = UTU Covariant Contravariant Contravariant
type (>.>:<.<) = UTU Contravariant Contravariant Contravariant

instance Interpreted (->) (UTU ct cu t u u') where
	type Primary (UTU ct cu t u u') a = u :. t :. u' >>> a
	run ~(UTU x) = x
	unite = UTU
