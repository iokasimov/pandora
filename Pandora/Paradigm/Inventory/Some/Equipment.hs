{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Inventory.Some.Equipment (Equipment (..), retrieve) where

import Pandora.Core.Interpreted (Schematic, Interpreted (Primary, run, unite))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<---), (<----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Comonad (Comonad)
import Pandora.Paradigm.Algebraic ()
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Controlflow.Effect.Adaptable (Adaptable (adapt))
import Pandora.Paradigm.Inventory.Ability.Gettable (Gettable (Getting, get))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))

newtype Equipment e a = Equipment (e :*: a)

instance Covariant (->) (->) (Equipment e) where
	f <-|- Equipment x = Equipment <---- f <-|- x

instance Traversable (->) (->) (Equipment e) where
	f <<- Equipment x = Equipment <-|-- f <<- x

instance Extendable (->) (Equipment e) where
	f <<= Equipment (e :*: x) = Equipment . (:*:) e . f . Equipment <---- e :*: x

instance Interpreted (->) (Equipment e) where
	type Primary (Equipment e) a = e :*: a
	run ~(Equipment x) = x
	unite = Equipment

type instance Schematic Comonad (Equipment e) = (<:.>) ((:*:) e)

type Equipped e t = Adaptable (Equipment e) (->) t

instance {-# OVERLAPS #-} Extendable (->) u => Extendable (->) ((:*:) e <:.> u) where
	f <<= TU (e :*: x) = TU . (:*:) e <--- f . TU . (:*:) e <<= x

retrieve :: Equipped e t => t a -> e
retrieve = attached . run @(->) @(Equipment _) . adapt

instance Gettable Equipment where
	type Getting Equipment e output = Equipment e output -> e
	get (Equipment (e :*: _)) = e
