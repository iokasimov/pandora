{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary.Functor (module Exports, note, left, right, this, that, here, there, branches, match) where

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

import Pandora.Core.Functor (type (~>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Adjoint (Adjoint ((-|), (|-)))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Structure.Ability.Monotonic (reduce)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Convert))

instance Adjoint (Product s) ((->) s) where
	(-|) :: a -> ((s :*: a) -> b) -> (s -> b)
	x -| f = \s -> f $ s :*: x
	(|-) :: (s :*: a) -> (a -> s -> b) -> b
	~(s :*: x) |- f = f x s

instance Morphable (Convert Maybe) (Conclusion e) where
	type Morphing (Convert Maybe) (Conclusion e) = Maybe
	morphing = conclusion (Nothing !) Just . extract . run

note :: e -> Maybe ~> Conclusion e
note x = reduce (\y _ -> Success y) (Failure x)

left :: Wye ~> Maybe
left (Both ls _) = Just ls
left (Left ls) = Just ls
left (Right _) = Nothing
left End = Nothing

right :: Wye ~> Maybe
right (Both _ rs) = Just rs
right (Left _) = Nothing
right (Right rs) = Just rs
right End = Nothing

this :: These e ~> Maybe
this (This x) = Just x
this (That _) = Nothing
this (These _ x) = Just x

that :: These e a -> Maybe e
that (This _) = Nothing
that (That x) = Just x
that (These y _) = Just y

here :: Wedge e a -> Maybe e
here Nowhere = Nothing
here (Here x) = Just x
here (There _) = Nothing

there :: Wedge e ~> Maybe
there Nowhere = Nothing
there (Here _) = Nothing
there (There x) = Just x

branches :: Maybe a -> Maybe a -> Wye a
branches (Just x) (Just y) = Both x y
branches Nothing (Just y) = Right y
branches (Just x) Nothing = Left x
branches Nothing Nothing = End

match :: Predicate a -> (a -> r) -> a -> r -> r :*: a
match (Predicate p) f x r = p x ? f x :*: x $ r :*: x
