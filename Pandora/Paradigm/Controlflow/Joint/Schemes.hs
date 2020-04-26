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
