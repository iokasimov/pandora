{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Morphable where

import Pandora.Core.Functor (type (:=), type (~>), type (:=:=>))
import Pandora.Pattern.Category ((.), (/))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Setoid (Setoid)
import Pandora.Paradigm.Primary.Functor (Comparison)
import Pandora.Paradigm.Primary.Functor.Convergence (Convergence (Convergence))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate, equate)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))

class Morphable f t | f t -> t where
	type Morphing (f :: k) (t :: * -> *) :: * -> *
	morphing :: Tagged f <:.> t ~> Morphing f t

morph :: forall f t . Morphable f t => t ~> Morphing f t
morph = morphing . TU . Tag @f

premorph :: Morphable f t => Tagged f <:.> t ~> t
premorph = extract . run

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a

data Morph a = Rotate a | Into a | Insert a | Push a | Pop a | Delete a

data Occurrence a = All a | First a

rotate :: forall f t . Morphable (Rotate f) t => t ~> Morphing (Rotate f) t
rotate = morphing . TU . Tag @(Rotate f)

into :: forall f t . Morphable (Into f) t => t ~> Morphing (Into f) t
into = morphing . TU . Tag @(Into f)

insert :: forall f t a . (Morphable (Insert f) t, Morphing (Insert f) t ~ (Identity <:.:> t := (->))) => a :=:=> t
insert new xs = run / morph @(Insert f) xs / Identity new

item :: forall f t a . (Morphable f t, Morphing f t ~ (Identity <:.:> t := (->))) => a :=:=> t
item new xs = run / morph @f xs / Identity new

-- FIXME: doesn't work right now, quantified constraints in instances for Binary have ambigous variables
collate :: forall f t a . (Chain a, Morphable f t, Morphing f t ~ ((Identity <:.:> Comparison := (:*:)) <:.:> t := (->))) => a :=:=> t
collate new xs = run / morph @f xs / T_U (Identity new :*: Convergence (<=>))

delete :: forall f t a . (Setoid a, Morphable (Delete f) t, Morphing (Delete f) t ~ (Predicate <:.:> t := (->))) => a :=:=> t
delete x xs = run / morph @(Delete f) xs / equate x

filter :: forall f t a . (Morphable (Delete f) t, Morphing (Delete f) t ~ (Predicate <:.:> t := (->))) => Predicate a -> t a -> t a
filter p xs = run / morph @(Delete f) xs / p
