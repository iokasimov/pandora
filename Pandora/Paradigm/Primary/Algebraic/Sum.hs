module Pandora.Paradigm.Primary.Algebraic.Sum where

infixr 0 :+:

data (:+:) s a = Option s | Adoption a
