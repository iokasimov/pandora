module Pandora.Paradigm.Schemes.PTU where

import Pandora.Core.Interpreted (Interpreted (Primary, run, unite))

newtype PTU p t u a b = PTU (p (t a) (u b))

instance Interpreted (->) (PTU p t u a) where
	type Primary (PTU p t u a) b = p (t a) (u b)
	run ~(PTU x) = x
	unite = PTU
