{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Schemes (module Exports) where

import Pandora.Paradigm.Schemes.PQ_ as Exports
import Pandora.Paradigm.Schemes.P_T as Exports
import Pandora.Paradigm.Schemes.PTU as Exports
import Pandora.Paradigm.Schemes.U_T as Exports
import Pandora.Paradigm.Schemes.T_U as Exports
import Pandora.Paradigm.Schemes.UTU as Exports
import Pandora.Paradigm.Schemes.UT as Exports
import Pandora.Paradigm.Schemes.TUVW as Exports
import Pandora.Paradigm.Schemes.TUT as Exports
import Pandora.Paradigm.Schemes.TU as Exports

import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)

instance (Covariant (v <:.> t), Covariant (u <:.> w), Adjoint t u, Adjoint v w)
	=> Adjoint (v <:.> t) (u <:.> w) where
		TU y |- g = y |- (|- run . g)
		x -| f = TU $ x -| (-| f . TU)

instance (Covariant (v <:.> t), Covariant (w <.:> u), Adjoint t u, Adjoint v w)
	=> Adjoint (v <:.> t) (w <.:> u) where
		TU t |- g = t |- (|- run . g)
		x -| f = UT $ x -| (-| f . TU)

instance (Covariant (t <.:> v), Covariant (w <.:> u), Adjoint t u, Adjoint v w)
	=> Adjoint (t <.:> v) (w <.:> u) where
		UT t |- g = t |- (|- run . g)
		x -| f = UT $ x -| (-| f . UT)

instance (Covariant (t <.:> v), Covariant (w <:.> u) , Adjoint v u, Adjoint t w)
	=> Adjoint (t <.:> v) (w <:.> u) where
		UT t |- g = t |- (|- run . g)
		x -| f = TU $ x -| (-| f . UT)

instance (Covariant ((t <:<.>:> u) t'), Covariant ((v <:<.>:> w) v')
	, Adjoint t w, Adjoint t' v', Adjoint t v, Adjoint u v, Adjoint v' t')
	=> Adjoint ((t <:<.>:> u) t') ((v <:<.>:> w) v') where
		TUT t |- g = t |- (|- (|- run . g))
		x -| f = TUT $ x -| (-| (-| f . TUT))
