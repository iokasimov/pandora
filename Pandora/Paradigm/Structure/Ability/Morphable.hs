{-# LANGUAGE AllowAmbiguousTypes #-}
module Pandora.Paradigm.Structure.Ability.Morphable where

import Pandora.Core.Functor (type (>>>>>>), type (>>>>>>>), type (~>), type (:=:=>))
import Pandora.Core.Interpreted (run)
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Setoid (Setoid)
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)))
import Pandora.Paradigm.Algebraic (extract)
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe)
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, equate)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

-- type Morphable :: k -> (* -> *) -> Constraint
class Morphable (mod :: k) struct | mod struct -> struct where
	type Morphing (mod :: k) (struct :: * -> *) :: * -> *
	morphing :: Tagged mod <::> struct ~> Morphing mod struct

type Morphed mod struct result = (Morphable mod struct, Morphing mod struct ~ result)

morph :: forall mod struct . Morphable mod struct => struct ~> Morphing mod struct
morph = morphing . TT . Tag @mod

premorph :: Morphable mod struct => Tagged mod <::> struct ~> struct
premorph = extract . run

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a

data Morph a = Rotate a | Into a | Insert a | Push a | Pop a | Delete a | Find a | Lookup a | Vary a | Key a | Element a

data Occurrence a = All a | First a

rotate :: forall mod struct . Morphable (Rotate mod) struct => struct ~> Morphing (Rotate mod) struct
rotate = morphing . TT . Tag @(Rotate mod)

into :: forall mod struct . Morphable (Into mod) struct => struct ~> Morphing (Into mod) struct
into = morphing . TT . Tag @(Into mod)

insert :: forall mod struct a . Morphed (Insert mod) struct (Exactly <:.:> struct >>>>>> (->)) => a :=:=> struct
insert new xs = run <-- morph @(Insert mod) xs <-- Exactly new

item :: forall mod struct a . Morphed mod struct (Exactly <:.:> struct >>>>>> (->)) => a :=:=> struct
item new xs = run <-- morph @mod xs <-- Exactly new

collate :: forall mod struct a . (Chain a, Morphed mod struct ((Exactly <:.:> Comparison >>>>>> (:*:)) <:.:> struct >>>>>> (->))) => a :=:=> struct
collate new xs = run <-- morph @mod xs <-- T_U (Exactly new :*: Convergence (<=>))

delete :: forall mod struct a . (Setoid a, Morphed (Delete mod) struct (Predicate <:.:> struct >>>>>> (->))) => a :=:=> struct
delete x xs = run <-- morph @(Delete mod) xs <-- equate x

filter :: forall mod struct a . (Morphed (Delete mod) struct (Predicate <:.:> struct >>>>>> (->))) => Predicate a -> struct a -> struct a
filter p xs = run <-- morph @(Delete mod) xs <-- p

find :: forall mod struct result a . (Morphed (Find mod) struct (Predicate <:.:> result >>>>>> (->))) => Predicate a -> struct a -> result a
find p xs = run <-- morph @(Find mod) xs <-- p

lookup :: forall mod key struct a . (Morphed (Lookup mod) struct ((->) key <::> Maybe)) => key -> struct a -> Maybe a
lookup key struct = run <-- morph @(Lookup mod) struct <-- key

vary :: forall mod key value struct . (Morphed (Vary mod) struct (((:*:) key <::> Exactly) <:.:> struct >>>>>>> (->))) => key -> value -> struct value -> struct value
vary key value xs = run <-- morph @(Vary mod) @struct xs <-- TT (key :*: Exactly value)
