module Pandora.Paradigm.Schemes.PTU where

newtype PTU p t u a b = PTU (p (t a) (u b))
