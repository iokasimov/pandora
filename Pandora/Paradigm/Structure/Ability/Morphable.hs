{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Paradigm.Structure.Ability.Morphable where

import Pandora.Core.Functor (type (~>), type (:=:=>))
import Pandora.Pattern.Category ((.), ($), (/))
import Pandora.Pattern.Functor.Covariant (Covariant)
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run)
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U)

class Morphable f t where
	type Morphing (f :: k) (t :: * -> *) :: * -> *
	morphing :: Tagged f <:.> t ~> Morphing f t

morph :: forall f t . Morphable f t => t ~> Morphing f t
morph = morphing . TU . Tag @f

premorph :: Morphable f t => Tagged f <:.> t ~> t
premorph = extract . run

data Walk a = Preorder a | Inorder a | Postorder a | Levelorder a

data Morph a = Rotate a | Into a | Prepend a

rotate :: forall f t . Morphable (Rotate f) t => t ~> Morphing (Rotate f) t
rotate = morphing . TU . Tag @(Rotate f)

into :: forall f t . Morphable (Into f) t => t ~> Morphing (Into f) t
into = morphing . TU . Tag @(Into f)

prepend :: forall f t a . (Morphable (Prepend f) t, Morphing (Prepend f) t ~ T_U Covariant Covariant (->) Identity t) => a :=:=> t
prepend new xs = run / morphing (TU $ Tag @(Prepend f) xs) / Identity new
