module Pandora.Pattern.Functor.Covariant where

import Pandora.Core.Functor (type (:.), type (:=), type (<:=))
import Pandora.Pattern.Semigroupoid (Semigroupoid ((.)))

infixl 4 -<$>-
infixl 3 -<<$$>-, -<$$>>-

-- TODO: use -<$>- instead of comap here
{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: comap identity ≡ identity
> * Interpreted of morphisms: comap (f . g) ≡ comap f . comap g
-}

class (Semigroupoid source, Semigroupoid target) => Covariant t source target where
	(-<$>-) :: source a b -> target (t a) (t b)
	
(-<$$>-) :: forall t u category a b 
	. (Covariant u category category, Covariant t category category) 
	=> category a b -> category (t (u a)) (t (u b))
(-<$$>-) s = ((-<$>-) ((-<$>-) @u @category @category s))

(-<<$$>-) :: forall t u source target a b 
	. (Covariant u source source, Covariant t source target) 
	=> source a b -> target (t (u a)) (t (u b))
(-<<$$>-) s = ((-<$>-) ((-<$>-) @u @source @source s))

(-<$$>>-) :: forall t u source target a b 
	. (Covariant u source target, Covariant t target target) 
	=> source a b -> target (t (u a)) (t (u b))
(-<$$>>-) s = ((-<$>-) ((-<$>-) @u @source @target s))

-- TODO: Figure out how to work with hidden type variables
-- to put intermediate category `between`

(-<$$$>-) :: forall t u v category a b
	. (Covariant t category category, Covariant u category category, Covariant v category category) 
	=> category a b -> category (t (u (v a))) (t (u (v b)))
(-<$$$>-) s = ((-<$>-) @t @category @category ((-<$>-) @u @category @category ((-<$>-) @v @category @category s)))

(-<$$$$>-) :: forall t u v w category a b
	. (Covariant t category category, Covariant u category category, Covariant v category category, Covariant w category category) 
	=> category a b -> category (t (u (v (w a)))) (t (u (v (w b))))
(-<$$$$>-) s = ((-<$>-) @t @category @category ((-<$>-) @u @category @category ((-<$>-) @v @category @category ((-<$>-) @w @category @category s))))
