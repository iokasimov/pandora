{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Primary (module Exports) where

import Pandora.Paradigm.Primary.Transformer as Exports
import Pandora.Paradigm.Primary.Functor as Exports
import Pandora.Paradigm.Primary.Object as Exports

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing), Morph (Into))

instance Morphable (Into (Left Maybe)) Wye where
	type Morphing (Into (Left Maybe)) Wye = Maybe
	morphing (extract . run -> Both ls _) = Just ls
	morphing (extract . run -> Left ls) = Just ls
	morphing (extract . run -> Right _) = Nothing
	morphing (extract . run -> End) = Nothing

instance Morphable (Into (Right Maybe)) Wye where
	type Morphing (Into (Right Maybe)) Wye = Maybe
	morphing (extract . run -> Both _ rs) = Just rs
	morphing (extract . run -> Left _) = Nothing
	morphing (extract . run -> Right rs) = Just rs
	morphing (extract . run -> End) = Nothing

instance Morphable (Into (This Maybe)) (These e) where
	type Morphing (Into (This Maybe)) (These e) = Maybe
	morphing (extract . run -> This x) = Just x
	morphing (extract . run -> That _) = Nothing
	morphing (extract . run -> These _ x) = Just x

instance Morphable (Into (There Maybe)) (Wedge e) where
	type Morphing (Into (There Maybe)) (Wedge e) = Maybe
	morphing (extract . run -> Nowhere) = Nothing
	morphing (extract . run -> Here _) = Nothing
	morphing (extract . run -> There x) = Just x
