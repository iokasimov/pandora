module Pandora.Paradigm.Schemes.TUVW (TUVW (..)) where

import Pandora.Core.Functor (type (:.), type (>>>))
import Pandora.Core.Interpreted (Interpreted (Primary, run, unite))

newtype TUVW ct cu cv cw t u v w a = TUVW (t :. u :. v :. w >>> a)

instance Interpreted (->) (TUVW ct cu cv cw t u v w) where
	type Primary (TUVW ct cu cv cw t u v w) a = t :. u :. v :. w >>> a
	run ~(TUVW x) = x
	unite = TUVW
