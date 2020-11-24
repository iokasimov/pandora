module Pandora.Paradigm.Schemes.T_U where

import Pandora.Paradigm.Primary.Functor.Product (type (:*:))

newtype T_U ct cu t u a = T_U (t a :*: u a)
