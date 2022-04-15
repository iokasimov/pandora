{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Schemes (module Exports) where

import Pandora.Paradigm.Schemes.PQ_ as Exports
import Pandora.Paradigm.Schemes.P_Q_T as Exports
import Pandora.Paradigm.Schemes.P_T as Exports
import Pandora.Paradigm.Schemes.PTU as Exports
import Pandora.Paradigm.Schemes.U_T as Exports
import Pandora.Paradigm.Schemes.T_U as Exports
import Pandora.Paradigm.Schemes.UTU as Exports
import Pandora.Paradigm.Schemes.UT as Exports
import Pandora.Paradigm.Schemes.TUVW as Exports
import Pandora.Paradigm.Schemes.TUT as Exports
import Pandora.Paradigm.Schemes.TU as Exports
import Pandora.Paradigm.Schemes.TT as Exports

import Pandora.Core.Interpreted (run)
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))

instance (Covariant (->) (->) (v <:.> t), Covariant (->) (->) (u <:.> w), Adjoint (->) (->) t u, Adjoint (->) (->) v w)
	=> Adjoint (->) (->) (v <:.> t) (u <:.> w) where
		g |- TU y = (run . g |-) |- y
		f -| x = TU <-- (f . TU -|) -| x

instance (Covariant (->) (->) (v <:.> t), Covariant (->) (->) (w <.:> u), Adjoint (->) (->) t u, Adjoint (->) (->) v w)
	=> Adjoint (->) (->) (v <:.> t) (w <.:> u) where
		g |- TU t = (run . g |-) |- t
		f -| x = UT <-- (f . TU -|) -| x

instance (Covariant (->) (->) (t <.:> v), Covariant (->) (->) (w <.:> u), Adjoint (->) (->) t u, Adjoint (->) (->) v w)
	=> Adjoint (->) (->) (t <.:> v) (w <.:> u) where
		g |- UT t =  (run . g |-) |- t
		f -| x = UT <-- (f . UT -|) -| x

instance (Covariant (->) (->) (t <.:> v), Covariant (->) (->) (w <:.> u), Adjoint (->) (->) v u, Adjoint (->) (->) t w)
	=> Adjoint (->) (->) (t <.:> v) (w <:.> u) where
		g |- UT x = (run . g |-) |- x
		f -| x = TU <-- (f . UT -|) -| x

instance (Covariant (->) (->) ((t <:<.>:> u) t'),  Covariant (->) (->) ((v <:<.>:> w) v'), Adjoint (->) (->) t w, Adjoint (->) (->) t' v', Adjoint (->) (->) t v, Adjoint (->) (->) u v, Adjoint (->) (->) v' t')
	=> Adjoint (->) (->) ((t <:<.>:> u) t') ((v <:<.>:> w) v') where
		g |- TUT x = ((run . g |-) |-) |- x
		f -| x = TUT <-- ((f . TUT -|) -|) -| x
