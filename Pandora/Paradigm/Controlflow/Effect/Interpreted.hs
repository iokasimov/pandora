module Pandora.Paradigm.Controlflow.Effect.Interpreted (Schematic, Interpreted (..)) where

type family Schematic (c :: (* -> *) -> k) (t :: * -> *) (u :: * -> *) = (r :: * -> *) | r -> t u

class Interpreted t where
	{-# MINIMAL run #-}
	type Primary t a :: *
	run :: t a -> Primary t a
