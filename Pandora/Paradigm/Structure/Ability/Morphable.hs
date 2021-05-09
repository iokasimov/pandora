{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Morphable where

import Pandora.Core.Functor (type (:=), type (~>), type (:=:=>))
import Pandora.Pattern.Category ((.), (#))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Setoid (Setoid)
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, equate)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

class Morphable mod struct | mod struct -> struct where
	type Morphing (mod :: k) (struct :: * -> *) :: * -> *
	morphing :: Tagged mod <:.> struct ~> Morphing mod struct

type Morphed mod struct result = (Morphable mod struct, Morphing mod struct ~ result)

morph :: forall mod struct . Morphable mod struct => struct ~> Morphing mod struct
morph = morphing . TU . Tag @mod

premorph :: Morphable mod struct => Tagged mod <:.> struct ~> struct
premorph = extract . run

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a

data Morph a = Rotate a | Into a | Insert a | Push a | Pop a | Delete a | Find a | Lookup a | Vary a | Key a | Element a

data Occurrence a = All a | First a

data Vertical a = Up a | Down a

rotate :: forall mod struct . Morphable (Rotate mod) struct => struct ~> Morphing (Rotate mod) struct
rotate = morphing . TU . Tag @(Rotate mod)

into :: forall mod struct . Morphable (Into mod) struct => struct ~> Morphing (Into mod) struct
into = morphing . TU . Tag @(Into mod)

insert :: forall mod struct a . Morphed (Insert mod) struct (Identity <:.:> struct := (->)) => a :=:=> struct
insert new xs = run # morph @(Insert mod) xs # Identity new

item :: forall mod struct a . Morphed mod struct (Identity <:.:> struct := (->)) => a :=:=> struct
item new xs = run # morph @mod xs # Identity new

collate :: forall mod struct a . (Chain a, Morphed mod struct ((Identity <:.:> Comparison := (:*:)) <:.:> struct := (->))) => a :=:=> struct
collate new xs = run # morph @mod xs # T_U (Identity new :*: Convergence (<=>))

delete :: forall mod struct a . (Setoid a, Morphed (Delete mod) struct (Predicate <:.:> struct := (->))) => a :=:=> struct
delete x xs = run # morph @(Delete mod) xs # equate x

filter :: forall mod struct a . (Morphed (Delete mod) struct (Predicate <:.:> struct := (->))) => Predicate a -> struct a -> struct a
filter p xs = run # morph @(Delete mod) xs # p

find :: forall mod struct result a . (Morphed (Find mod) struct (Predicate <:.:> result := (->))) => Predicate a -> struct a -> result a
find p xs = run # morph @(Find mod) xs # p

lookup :: forall mod key struct a . (Morphed (Lookup mod) struct ((->) key <:.> Maybe)) => key -> struct a -> Maybe a
lookup key struct = run # morph @(Lookup mod) struct # key

vary :: forall mod key value struct . (Morphed (Vary mod) struct ((Product key <:.> Identity) <:.:> struct := (->))) => key -> value -> struct value -> struct value
vary key value xs = run # morph @(Vary mod) @struct xs # TU (key :*: Identity value)
