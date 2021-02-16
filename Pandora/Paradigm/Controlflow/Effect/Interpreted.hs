module Pandora.Paradigm.Controlflow.Effect.Interpreted where

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))

type family Schematic (c :: (* -> *) -> k) (t :: * -> *) = (r :: (* -> *) -> * -> *) | r -> t

class Interpreted t where
	{-# MINIMAL run, unite #-}
	type Primary t a :: *
	run :: t a -> Primary t a
	unite :: Primary t a -> t a

	(||=) :: (Primary t a -> Primary t b) -> t a -> t b
	(||=) f = unite . f . run

	(=||) :: (t a -> t b) -> Primary t a -> Primary t b
	(=||) f = run . f . unite

(-=:) :: (Liftable t, Interpreted (t u), Interpreted (t v), Covariant u)
	=> (t u a -> t v b) -> u a -> Primary (t v) b
(-=:) f = run . f . lift
