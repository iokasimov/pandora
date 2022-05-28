module Pandora.Pattern.Transformation (module Exports, Component (..), Transformation (..)) where

import Pandora.Pattern.Transformation.Lowerable as Exports
import Pandora.Pattern.Transformation.Liftable as Exports
import Pandora.Pattern.Transformation.Hoistable as Exports

import Pandora.Pattern.Functor (Functor)

-- TODO: Category/Semigroupoid and Functor constrants
class Component category t u where
	component :: category (t a) (u a)

type Liftable_ category u t = Component category u (t u)
type Lowerable_ category t u = Component category (t u) u

lift_ :: Liftable_ category u t => category (u a) (t u a)
lift_ = component

lower_ :: Lowerable_ category t u => category (t u a) (u a)
lower_ = component

-- Law: (-|-) morphism . component = component . (-|-) morphism
-- TODO: natural transformations on semigroupoids?
class (Functor source target t, Functor source target u) => Transformation source target t u where
	(|-|) :: source a b -> target (t a) (u b)
