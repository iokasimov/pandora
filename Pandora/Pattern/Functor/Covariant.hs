{-# LANGUAGE AllowAmbiguousTypes #-}

module Pandora.Pattern.Functor.Covariant where

import Pandora.Pattern.Semigroupoid (Semigroupoid)

infixl 4 <$>
infixl 3 <$$>

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: (identity <$>) ≡ identity
> * Interpreted of morphisms: (f . g <$>) ≡ (f <$>) . (g <$>)
-}

class (Semigroupoid source, Semigroupoid target) => Covariant source target t where
	(<$>) :: source a b -> target (t a) (t b)

(<$$>) :: forall source between target t u a b
	. (Covariant source between u, Covariant between target t)
	=> source a b -> target (t (u a)) (t (u b))
(<$$>) s = ((<$>) ((<$>) @source @between @u s))

(<$$$>) :: forall source between1 between2 target t u v a b
	. (Covariant source between1 v, Covariant between1 between2 u, Covariant between2 target t)
	=> source a b -> target (t (u (v a))) (t (u (v b)))
(<$$$>) s = ((<$>) @between2 @target ((<$>) @between1 @between2 @u ((<$>) @source @between1 @v s)))

(-<$$$$>-) :: forall category t u v w a b
	. (Covariant category category t, Covariant category category u, Covariant category category v, Covariant category category w)
	=> category a b -> category (t (u (v (w a)))) (t (u (v (w b))))
(-<$$$$>-) s = ((<$>) @category @category @t ((<$>) @category @category @u ((<$>) @category @category @v ((<$>) @category @category @w s))))
