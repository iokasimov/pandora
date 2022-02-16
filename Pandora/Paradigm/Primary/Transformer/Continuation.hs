{-# LANGUAGE UndecidableInstances #-}
module Pandora.Paradigm.Primary.Transformer.Continuation where

import Pandora.Core.Functor (type (:.), type (:=), type (::|:.))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--))
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Functor.Monoidal (Monoidal)
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Monad (Monad)
import Pandora.Pattern.Transformer.Liftable (Liftable (lift))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (Interpreted (Primary, run, unite, (<~)))
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%), type (-->))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:))
import Pandora.Paradigm.Primary.Algebraic (point)

newtype Continuation r t a = Continuation ((->) ::|:. a :. t := r)

instance Interpreted (->) (Continuation r t) where
	type Primary (Continuation r t) a = (->) ::|:. a :. t := r
	run ~(Continuation x) = x
	unite = Continuation

instance Covariant (->) (->) t => Covariant (->) (->) (Continuation r t) where
	f <-|- Continuation continuation = Continuation <-- continuation . (. f)

instance Covariant (->) (->) t => Bindable (->) (Continuation r t) where
	f =<< x = Continuation <-- \g -> x <~ \y -> f y <~ g

-- TODO: Define Monoidal (-->) (-->) (:*:) (:*:) (Continuation r t)

-- instance (Monoidal (-->) (-->) (:*:) (:*:) t, Monad (->) t) => Monad (->) (Continuation r t) where

instance (forall u . Bindable (->) u) => Liftable (->) (Continuation r) where
	lift = Continuation . (%) (=<<)

-- | Call with current continuation
cwcc :: ((a -> Continuation r t b) -> Continuation r t a) -> Continuation r t a
cwcc f = Continuation <-- \g -> (<~ g) . f <-- Continuation . constant . g

-- | Delimit the continuation of any 'shift'
reset :: (forall u . Bindable (->) u, Monad (->) t) => Continuation r t r -> Continuation s t r
reset = lift . (<~ point)

-- | Capture the continuation up to the nearest enclosing 'reset' and pass it
shift :: Monoidal (-->) (-->) (:*:) (:*:) t => ((a -> t r) -> Continuation r t r) -> Continuation r t a
shift f = Continuation <-- (<~ point) . f

interruptable :: Monoidal (-->) (-->) (:*:) (:*:) t => ((a -> Continuation a t a) -> Continuation a t a) -> t a
interruptable = (<~ point) . cwcc
