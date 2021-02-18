{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Functor (module Exports, branches, match) where

import Pandora.Paradigm.Primary.Functor.Fix as Exports
import Pandora.Paradigm.Primary.Functor.Equivalence as Exports
import Pandora.Paradigm.Primary.Functor.Predicate as Exports
import Pandora.Paradigm.Primary.Functor.These as Exports
import Pandora.Paradigm.Primary.Functor.Validation as Exports
import Pandora.Paradigm.Primary.Functor.Wedge as Exports
import Pandora.Paradigm.Primary.Functor.Wye as Exports
import Pandora.Paradigm.Primary.Functor.Edges as Exports
import Pandora.Paradigm.Primary.Functor.Conclusion as Exports
import Pandora.Paradigm.Primary.Functor.Maybe as Exports
import Pandora.Paradigm.Primary.Functor.Endo as Exports
import Pandora.Paradigm.Primary.Functor.Proxy as Exports
import Pandora.Paradigm.Primary.Functor.Tagged as Exports
import Pandora.Paradigm.Primary.Functor.Product as Exports
import Pandora.Paradigm.Primary.Functor.Constant as Exports
import Pandora.Paradigm.Primary.Functor.Identity as Exports
import Pandora.Paradigm.Primary.Functor.Function as Exports

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Object.Boolean ((?))

instance Adjoint (Product s) ((->) s) where
	(-|) :: a -> ((s :*: a) -> b) -> (s -> b)
	x -| f = \s -> f $ s :*: x
	(|-) :: (s :*: a) -> (a -> s -> b) -> b
	~(s :*: x) |- f = f x s

branches :: Maybe a -> Maybe a -> Wye a
branches (Just x) (Just y) = Both x y
branches Nothing (Just y) = Right y
branches (Just x) Nothing = Left x
branches Nothing Nothing = End

match :: Predicate a -> (a -> r) -> a -> r -> r :*: a
match (Predicate p) f x r = p x ? f x :*: x $ r :*: x
