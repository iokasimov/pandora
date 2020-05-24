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

instance (Covariant (TUT Covariant Covariant Covariant t u t'), Covariant (TUT Covariant Covariant Covariant v w v')
	, Adjoint t w, Adjoint t' v', Adjoint t v, Adjoint u v, Adjoint v' t')
	=> Adjoint (TUT Covariant Covariant Covariant t u t') (TUT Covariant Covariant Covariant v w v') where
		TUT t |- g = t |- (|- (|- run . g))
		x -| f = TUT $ x -| (-| (-| f . TUT))
