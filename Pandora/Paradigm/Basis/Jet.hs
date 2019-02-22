module Pandora.Paradigm.Basis.Jet (Jet (..)) where

data Jet t a = a :- Jet t (t a)
