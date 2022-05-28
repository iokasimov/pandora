module Pandora.Pattern.Transformation (module Exports, Component (..), Transformation (..)) where

import Pandora.Pattern.Transformation.Lowerable as Exports
import Pandora.Pattern.Transformation.Liftable as Exports
import Pandora.Pattern.Transformation.Hoistable as Exports

import Pandora.Pattern.Functor (Functor)

-- TODO: Category/Semigroupoid and Functor constrants
class Component target t u where
	component :: target (t a) (u a)

-- Law: (-|-) morphism . component = component . (-|-) morphism
-- TODO: natural transformations on semigroupoids?
class (Functor source target t, Functor source target u) => Transformation source target t u where
	(|-|) :: source a b -> target (t a) (u b)
