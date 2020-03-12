module Pandora.Paradigm.Controlflow.Joint.Schemes.TUVW (TUVW (..)) where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Paradigm.Controlflow.Joint.Interpreted (Interpreted (Primary, run))

newtype TUVW ct cu cv cw t u v w a = TUVW (t :. u :. v :. w := a)

instance Interpreted (TUVW ct cu cv cw t u v w) where
	type Primary (TUVW ct cu cv cw t u v w) a = t :. u :. v :. w := a
	run (TUVW x) = x
