module Pattern.Object.Semilattice (Infimum (..), Supremum (..), Semilattice) where

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
> * Associativity: x /\ (y /\ z) ≡ (x /\ y) /\ z
> * Commutativity: x /\ y ≡ y /\ x
> * Idempotency: x /\ x ≡ x
-}

class Infimum a where
	{-# MINIMAL (/\) #-}
	(/\) :: a -> a -> a

{- |
> When providing a new instance, you should ensure it satisfies the three laws:
> * Associativity: x \/ (y \/ z) ≡ (x \/ y) \/ z
> * Commutativity: x \/ y ≡ y \/ x
> * Idempotency: x \/ x ≡ x
-}

class Supremum a where
	{-# MINIMAL (\/) #-}
	(\/) :: a -> a -> a

type family Semilattice constraint where
	Semilattice Infimum = ()
	Semilattice Supremum = ()
