module Pandora.Pattern.Functor.Covariant where

import Pandora.Core.Functor (type (:.), type (:=), type (<:=))
import Pandora.Pattern.Category (Category ((.)))

infixl 4 <$>, -<$>-, <$, $>
infixl 3 <$$>, -<<$$>-, -<$$>>-
infixl 2 <$$$>
infixl 1 <$$$$>

infixl 1 <&>
infixl 2 <&&>
infixl 3 <&&&>
infixl 4 <&&&&>

infixr 7 .#.., .#..., .#....

{- |
> When providing a new instance, you should ensure it satisfies:
> * Identity morphism: comap identity ≡ identity
> * Interpreted of morphisms: comap (f . g) ≡ comap f . comap g
-}

class Covariant (t :: * -> *) where
	{-# MINIMAL (<$>) #-}
	-- | Infix version of 'comap'
	(<$>) :: (a -> b) -> t a -> t b

	-- | Prefix version of '<$>'
	comap :: (a -> b) -> t a -> t b
	comap f x = f <$> x
	-- | Replace all locations in the input with the same value
	(<$) :: a -> t b -> t a
	x <$ z = (\_-> x) <$> z
	-- | Flipped version of '<$'
	($>) :: t a -> b -> t b
	x $> v = v <$ x
	-- | Discards the result of evaluation
	void :: t a -> t ()
	void x = () <$ x
	-- | Computing a value from a structure of values
	loeb :: t (a <:= t) -> t a
	loeb tt = let fix f = let x = f x in x in fix (\f -> (\g -> g f) <$> tt)
	-- | Flipped infix version of 'comap'
	(<&>) :: t a -> (a -> b) -> t b
	x <&> f = f <$> x

	-- | Infix versions of `comap` with various nesting levels
	(<$$>) :: Covariant u => (a -> b) -> t :. u := a -> t :. u := b
	f <$$> x = ((f <$>) <$>) x
	(<$$$>) :: (Covariant u, Covariant v)
		=> (a -> b) -> t :. u :. v := a -> t :. u :. v := b
	f <$$$> x = (((f <$>) <$>) <$>) x
	(<$$$$>) :: (Covariant u, Covariant v, Covariant w)
		=> (a -> b) -> t :. u :. v :. w := a -> t :. u :. v :. w := b
	f <$$$$> x = ((((f <$>) <$>) <$>) <$>) x

	-- | Infix flipped versions of `comap` with various nesting levels
	(<&&>) :: Covariant u => t :. u := a -> (a -> b) -> t :. u := b
	x <&&> f = f <$$> x
	(<&&&>) :: (Covariant u, Covariant v)
		=> t :. u :. v := a -> (a -> b) -> t :. u :. v := b
	x <&&&> f = f <$$$> x
	(<&&&&>) :: (Covariant u, Covariant v, Covariant w)
		=> t :. u :. v :. w := a -> (a -> b) -> t :. u :. v :. w := b
	x <&&&&> f = f <$$$$> x

	(.#..) :: (t ~ v a, Category v)
		=> v c d -> v a :. v b := c -> v a :. v b := d
	f .#.. g = (f .) <$> g

	(.#...) :: (t ~ v a, t ~ v b, Category v, Covariant (v a), Covariant (v b))
		=> v d e -> v a :. v b :. v c := d -> v a :. v b :. v c := e
	f .#... g = (f .) <$$> g

	(.#....) :: (t ~ v a, t ~ v b, t ~ v c, Category v, Covariant (v a), Covariant (v b), Covariant (v c))
		=> v e f -> v a :. v b :. v c :. v d := e -> v a :. v b :. v c :. v d := f
	f .#.... g = (f .) <$$$> g

	(<$$) :: Covariant u => b -> t :. u := a -> t :. u := b
	x <$$ s = (\_-> x) <$$> s

	(<$$$) :: (Covariant u, Covariant v) => b -> t :. u :. v := a -> t :. u :. v := b
	x <$$$ s = (\_-> x) <$$$> s

	(<$$$$) :: (Covariant u, Covariant v, Covariant w) => b -> t :. u :. v :. w := a -> t :. u :. v :. w := b
	x <$$$$ s = (\_-> x) <$$$$> s

	($$>) :: Covariant u => t :. u := a -> b -> t :. u := b
	s $$> x = (\_-> x) <$$> s

	($$$>) :: (Covariant u, Covariant v) => t :. u :. v := a -> b -> t :. u :. v := b
	s $$$> x = (\_-> x) <$$$> s

	($$$$>) :: (Covariant u, Covariant v, Covariant w) => t :. u :. v :. w := a -> b -> t :. u :. v :. w := b
	s $$$$> x = (\_-> x) <$$$$> s

class (Category source, Category target) => Covariant_ t source target where
	(-<$>-) :: source a b -> target (t a) (t b)
	
(-<$$>-) :: forall t u category a b 
	. (Covariant_ u category category, Covariant_ t category category) 
	=> category a b -> category (t (u a)) (t (u b))
(-<$$>-) s = ((-<$>-) ((-<$>-) @u @category @category s))

(-<<$$>-) :: forall t u source target a b 
	. (Covariant_ u source source, Covariant_ t source target) 
	=> source a b -> target (t (u a)) (t (u b))
(-<<$$>-) s = ((-<$>-) ((-<$>-) @u @source @source s))

(-<$$>>-) :: forall t u source target a b 
	. (Covariant_ u source target, Covariant_ t target target) 
	=> source a b -> target (t (u a)) (t (u b))
(-<$$>>-) s = ((-<$>-) ((-<$>-) @u @source @target s))

-- TODO: Figure out how to work with hidden type variables
-- to put intermediate category `between`
(-<$$$>-) :: forall t u v source target a b
	. (Covariant_ u source source, Covariant_ t source target, Covariant_ v target target) 
	=> source a b -> target (v (t (u a))) (v (t (u b)))
(-<$$$>-) s = ((-<$>-) ((-<$>-) @t @source @target ((-<$>-) @u @source @source s)))
