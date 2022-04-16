{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Structure.Ability.Substructure where

import Pandora.Core.Interpreted (run, unite, (=#-))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))
import Pandora.Pattern.Category ((<--), (<---), (<----))
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-|-)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (type (@>>>), view, replace)
import Pandora.Paradigm.Algebraic.Exponential ((%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), (<:*:>))
import Pandora.Paradigm.Algebraic ((>-||-), extract)
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged)
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left_, Right_))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Schemes.TU (type (<:.>))
import Pandora.Paradigm.Schemes.TT (type (<::>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))

type Substructured segment source target = (Substructure segment source, Substance segment source ~ target)

class Substructure segment (structure :: * -> *) where
	type Substance segment structure :: * -> *
	substructure :: (Tagged segment <:.> structure) @>>> Substance segment structure

	sub :: (Covariant (->) (->) structure) => structure @>>> Substance segment structure
	sub = (lift >-||-) . (lower @(->) <-|-|-) =#- substructure @segment @structure

tagstruct :: Covariant (->) (->) structure => (Tagged segment <:.> structure) @>>> structure
tagstruct = P_Q_T <-- \ts -> case lower ts of
	struct -> Store <--- struct :*: lift

instance (Covariant (->) (->) t, Covariant (->) (->) u) => Substructure Left_ (t <:*:> u) where
	type Substance Left_ (t <:*:> u) = t
	substructure = P_Q_T <-- \x -> case run <-- lower x of
		ls :*: rs -> Store <--- ls :*: lift . (<:*:> rs)

instance (Covariant (->) (->) t, Covariant (->) (->) u) => Substructure Right_ (t <:*:> u) where
	type Substance Right_ (t <:*:> u) = u
	substructure = P_Q_T <-- \x -> case run <-- lower x of
		ls :*: rs -> Store <--- rs :*: lift . (ls <:*:>)

data Segment a = Root a | Rest a | Branch a | Ancestors a | Forest a

instance Covariant (->) (->) t => Substructure Root (Exactly <:*:> t) where
	type Substance Root (Exactly <:*:> t) = Exactly
	substructure = (lower >-||-) . (lift @(->) <-|-|-) =#- sub @Left_

instance Covariant (->) (->) t => Substructure Rest (Exactly <:*:> t) where
	type Substance Rest (Exactly <:*:> t) = t
	substructure = (lower >-||-) . (lift @(->) <-|-|-) =#- sub @Right_

instance Covariant (->) (->) t => Substructure Root (Construction t) where
	type Substance Root (Construction t) = Exactly
	substructure = P_Q_T <-- \source -> case lower source of
		Construct x xs -> Store <--- Exactly x :*: lift . (Construct % xs) . extract

instance Covariant (->) (->) t => Substructure Rest (Construction t) where
	type Substance Rest (Construction t) = t <::> Construction t
	substructure = P_Q_T <-- \source -> case lower source of
		Construct x xs -> Store <--- unite xs :*: lift . Construct x . run

instance (Covariant (->) (->) t, Substructure i t) => Substructure (i Branch) (Construction t) where
	type Substance (i Branch) (Construction t) = Substance i t <::> Construction t
	substructure = P_Q_T <-- \source -> case lower source of
		Construct x xs -> Store <--- unite (view <-- sub @i <-- xs) :*: \target ->
			lift <---- Construct x <--- replace <-- run target <-- sub @i <-- xs
