module Pandora.Paradigm.Controlflow.Effect.Interpreted where

import Pandora.Pattern.Category ((.))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))

type family Schematic (c :: (* -> *) -> k) (t :: * -> *) = (r :: (* -> *) -> * -> *) | r -> t

class Interpreted t where
	{-# MINIMAL run #-}
	type Primary t a :: *
	run :: t a -> Primary t a

via :: (Liftable t, Interpreted (t u), Interpreted (t v), Covariant u)
	=> (t u a -> t v b) -> u a -> Primary (t v) b
via f = run . f . lift
