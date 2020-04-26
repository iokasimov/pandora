{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Controlflow.Joint.Schemes (module Exports) where

import Pandora.Paradigm.Controlflow.Joint.Schemes.UTU as Exports
import Pandora.Paradigm.Controlflow.Joint.Schemes.UT as Exports
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUVW as Exports
import Pandora.Paradigm.Controlflow.Joint.Schemes.TUT as Exports
import Pandora.Paradigm.Controlflow.Joint.Schemes.TU as Exports

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Pattern.Functor.Divariant (($))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (run)

instance (Covariant (TU Covariant Covariant v t), Covariant (TU Covariant Covariant u w), Adjoint t u, Adjoint v w)
	=> Adjoint (TU Covariant Covariant v t) (TU Covariant Covariant u w) where
		TU y |- g = y |- (|- run . g)
		x -| f = TU $ x -| (-| f . TU)

instance (Covariant (TU Covariant Covariant v t), Covariant (UT Covariant Covariant w u), Adjoint t u, Adjoint v w)
	=> Adjoint (TU Covariant Covariant v t) (UT Covariant Covariant w u) where
		TU y |- g = y |- (|- run . g)
		x -| f = UT $ x -| (-| f . TU)

instance (Covariant (UT Covariant Covariant t v), Covariant (UT Covariant Covariant w u), Adjoint t u, Adjoint v w)
	=> Adjoint (UT Covariant Covariant t v) (UT Covariant Covariant w u) where
		UT y |- g = y |- (|- run . g)
		x -| f = UT $ x -| (-| f . UT)

instance (Covariant (UT Covariant Covariant t v), Covariant (TU Covariant Covariant w u) , Adjoint v u, Adjoint t w)
	=> Adjoint (UT Covariant Covariant t v) (TU Covariant Covariant w u) where
		UT y |- g = y |- (|- run . g)
		x -| f = TU $ x -| (-| f . UT)
