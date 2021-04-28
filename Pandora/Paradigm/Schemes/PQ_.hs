module Pandora.Paradigm.Schemes.PQ_ where

newtype PQ_ p q a b = PQ_ (p a (q b a))
