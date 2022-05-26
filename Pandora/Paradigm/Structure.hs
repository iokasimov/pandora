{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure (module Exports) where

import Pandora.Paradigm.Structure.Ability as Exports
import Pandora.Paradigm.Structure.Interface as Exports
import Pandora.Paradigm.Structure.Modification as Exports
import Pandora.Paradigm.Structure.Some as Exports

import Pandora.Core.Interpreted (run, (<~))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), identity)
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant ((<-|-)))
import Pandora.Pattern.Transformation.Liftable (lift)
import Pandora.Pattern.Transformation.Lowerable (lower)
import Pandora.Pattern.Object.Semigroup ((+))
import Pandora.Paradigm.Inventory.Some.Optics ()
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Algebraic.Exponential ((%))
import Pandora.Paradigm.Algebraic.Product ((:*:) ((:*:)), type (<:*:>), attached)
import Pandora.Paradigm.Algebraic.Sum ((:+:) (Option, Adoption))
import Pandora.Paradigm.Algebraic (extract)
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Conclusion (Conclusion (Failure, Success), conclusion)
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Both, Left_, Right_, End))
import Pandora.Paradigm.Primary.Functor.Wedge (Wedge (Nowhere, Here, There))
import Pandora.Paradigm.Primary.Functor.These (These (This, That, These))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct))
import Pandora.Paradigm.Primary.Linear.Vector (Vector (Scalar, Vector))
import Pandora.Pattern.Morphism.Flip (Flip (Flip))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Schemes.TT (TT (TT))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))

instance Monotonic s a => Monotonic s (s :*: a) where
	reduce f r x = reduce f <-- f (attached x) r <-- extract x

instance Morphable (Into Maybe) (Conclusion e) where
	type Morphing (Into Maybe) (Conclusion e) = Maybe
	morphing = conclusion (constant Nothing) Just . premorph

instance Morphable (Into (Conclusion e)) Maybe where
	type Morphing (Into (Conclusion e)) Maybe = (->) e <:.> Conclusion e
	morphing (premorph -> Just x) = TU <-- \_ -> Success x
	morphing (premorph -> Nothing) = TU <-- \e -> Failure e

instance Morphable (Into (Flip Conclusion e)) Maybe where
	type Morphing (Into (Flip Conclusion e)) Maybe = (->) e <:.> Flip Conclusion e
	morphing (run . premorph -> Just x) = TU <-- \_ -> Flip <-- Failure x
	morphing (run . premorph -> Nothing) = TU <-- Flip . Success

instance Morphable (Into (Left_ Maybe)) Wye where
	type Morphing (Into (Left_ Maybe)) Wye = Maybe
	morphing (premorph -> Both ls _) = Just ls
	morphing (premorph -> Left_ ls) = Just ls
	morphing (premorph -> Right_ _) = Nothing
	morphing (premorph -> End) = Nothing

instance Morphable (Into (Right_ Maybe)) Wye where
	type Morphing (Into (Right_ Maybe)) Wye = Maybe
	morphing (premorph -> Both _ rs) = Just rs
	morphing (premorph -> Left_ _) = Nothing
	morphing (premorph -> Right_ rs) = Just rs
	morphing (premorph -> End) = Nothing

instance Morphable (Into (This Maybe)) (These e) where
	type Morphing (Into (This Maybe)) (These e) = Maybe
	morphing (premorph -> This x) = Just x
	morphing (premorph -> That _) = Nothing
	morphing (premorph -> These _ x) = Just x

instance Morphable (Into (That Maybe)) (Flip These a) where
	type Morphing (Into (That Maybe)) (Flip These a) = Maybe
	morphing (run . premorph -> This _) = Nothing
	morphing (run . premorph -> That x) = Just x
	morphing (run . premorph -> These y _) = Just y

instance Morphable (Into (Here Maybe)) (Flip Wedge a) where
	type Morphing (Into (Here Maybe)) (Flip Wedge a) = Maybe
	morphing (run . premorph -> Nowhere) = Nothing
	morphing (run . premorph -> Here x) = Just x
	morphing (run . premorph -> There _) = Nothing

instance Morphable (Into (There Maybe)) (Wedge e) where
	type Morphing (Into (There Maybe)) (Wedge e) = Maybe
	morphing (premorph -> Nowhere) = Nothing
	morphing (premorph -> Here _) = Nothing
	morphing (premorph -> There x) = Just x

instance Morphable (Into Wye) (Maybe <:*:> Maybe) where
	type Morphing (Into Wye) (Maybe <:*:> Maybe) = Wye
	morphing (run . premorph -> Just x :*: Just y) = Both x y
	morphing (run . premorph -> Nothing :*: Just y) = Right_ y
	morphing (run . premorph -> Just x :*: Nothing) = Left_ x
	morphing (run . premorph -> Nothing :*: Nothing) = End

instance Substructure Left_ Wye where
	type Substance Left_ Wye = Maybe
	substructure = P_Q_T <-- \new -> case lower new of
		End -> Store <--- Nothing :*: lift . resolve Left_ End
		Left_ x -> Store <--- Just x :*: lift . resolve Left_ End
		Right_ y -> Store <--- Nothing :*: lift . constant (Right_ y)
		Both x y -> Store <--- Just x :*: lift . resolve (Both % y) (Right_ y)

instance Substructure Right_ Wye where
	type Substance Right_ Wye = Maybe
	substructure = P_Q_T <-- \new -> case lower new of
		End -> Store <--- Nothing :*: lift . resolve Right_ End
		Left_ x -> Store <--- Nothing :*: lift . constant (Left_ x)
		Right_ y -> Store <--- Just y :*: lift . resolve Right_ End
		Both x y -> Store <--- Just y :*: lift . resolve (Both x) (Left_ x)

instance (Covariant (->) (->) t) => Substructure Rest (Tap t) where
	type Substance Rest (Tap t) = t
	substructure = P_Q_T <-- \tap -> case extract <-- run tap of
		Tap x xs -> Store <--- xs :*: lift . Tap x

instance Morphable (Into (Preorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Preorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing nonempty_binary = case premorph nonempty_binary of
		Construct x End -> Construct x Nothing
		Construct x (Left_ lst) -> Construct x . Just <-- into @(Preorder (Nonempty List)) lst
		Construct x (Right_ rst) -> Construct x . Just <-- into @(Preorder (Nonempty List)) rst
		Construct x (Both lst rst) -> Construct x . Just <-- into @(Preorder (Nonempty List)) lst + into @(Preorder (Nonempty List)) rst

instance Morphable (Into (Inorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Inorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing nonempty_binary = case premorph nonempty_binary of
		Construct x End -> Construct x Nothing
		Construct x (Left_ lst) -> into @(Inorder (Nonempty List)) lst + Construct x Nothing
		Construct x (Right_ rst) -> Construct x Nothing + into @(Inorder (Nonempty List)) rst
		Construct x (Both lst rst) -> into @(Inorder (Nonempty List)) lst + Construct x Nothing + into @(Inorder (Nonempty List)) rst

instance Morphable (Into (Postorder (Construction Maybe))) (Construction Wye) where
	type Morphing (Into (Postorder (Construction Maybe))) (Construction Wye) = Construction Maybe
	morphing nonempty_binary = case premorph nonempty_binary of
		Construct x End -> Construct x Nothing
		Construct x (Left_ lst) -> into @(Postorder (Nonempty List)) lst + Construct x Nothing
		Construct x (Right_ rst) -> into @(Postorder (Nonempty List)) rst + Construct x Nothing
		Construct x (Both lst rst) -> into @(Postorder (Nonempty List)) lst + into @(Postorder (Nonempty List)) rst + Construct x Nothing

-- instance Morphable (Into (o ds)) (Construction Wye) => Morphable (Into (o ds)) Binary where
	-- type Morphing (Into (o ds)) Binary = Maybe <:.> Morphing (Into (o ds)) (Construction Wye)
	-- morphing (premorph -> xs) = (into @(o ds) <-|-) =#- xs

instance Substructure Left_ (Flip (:*:) a) where
	type Substance Left_ (Flip (:*:) a) = Exactly
	substructure = P_Q_T <-- \product -> case run <-- lower product of
		s :*: x -> Store <--- Exactly s :*: lift . Flip . (:*: x) . extract

instance Substructure Right_ ((:*:) s) where
	type Substance Right_ ((:*:) s) = Exactly
	substructure = P_Q_T <-- \product -> case lower product of
		s :*: x -> Store <--- Exactly x :*: lift . (s :*:) . extract

instance Accessible s (s :*: a) where
	access = P_Q_T <-- \(s :*: x) -> Store <--- Exactly s :*: (:*: x) . extract

instance Accessible a (s :*: a) where
	access = P_Q_T <-- \(s :*: x) -> Store <--- Exactly x :*: (s :*:) . extract

instance {-# OVERLAPS #-} Accessible b a => Accessible b (s :*: a) where
	access = access @b . access @a

-- TODO: Causes overlapping instances error when target is (a :*: b), it's better to use some wrapper instead
-- instance {-# OVERLAPS #-} (Accessible a s, Accessible b s) => Accessible (a :*: b) s where
	-- access = mult @(-->) @(:*:) @(:*:) <~ (access @a :*: access @b)

instance Accessible a (Exactly a) where
	access = P_Q_T <-- \(Exactly x) -> Store <--- Exactly x :*: identity

instance Possible a (Maybe a) where
	perhaps = P_Q_T <-- \x -> Store <--- x :*: identity

instance {-# OVERLAPS #-} Possible a (o :+: a) where
	perhaps = P_Q_T <-- \case
		Option s -> Store <--- Nothing :*: (resolve @a @(Maybe a) <-- Adoption <-- Option s)
		Adoption x -> Store <--- Just x :*: (resolve @a @(Maybe a) <-- Adoption <-- Adoption x)

instance {-# OVERLAPS #-} Possible o (o :+: a) where
	perhaps = P_Q_T <-- \case
		Option s -> Store <--- Just s :*: (resolve @o @(Maybe o) <-- Option <-- Option s)
		Adoption x -> Store <--- Nothing :*: (resolve @o @(Maybe o) <-- Option <-- Adoption x)

instance Accessible target source => Possible target (Maybe source) where
	perhaps = let lst = access @target @source in P_Q_T <-- \case
		Just source -> let (Exactly target :*: its) = run (lst <~ source) in
			Store <--- Just target :*: (its . Exactly <-|-)
		Nothing -> Store <--- Nothing :*: \_ -> Nothing

instance Accessible (Maybe target) source => Possible target source where
	perhaps = let lst = access @(Maybe target) @source in P_Q_T <-- \source ->
		let target :*: imts = run (lst <~ source) in
			Store <--- extract target :*: imts . Exactly

instance Morphable (Into List) (Vector r) where
	type Morphing (Into List) (Vector r) = List
	morphing (premorph -> Scalar x) = TT . Just <-- Construct x Nothing
	morphing (premorph -> Vector x xs) = item @Push x <-- into @List xs

instance Morphable (Into (Construction Maybe)) (Vector r) where
	type Morphing (Into (Construction Maybe)) (Vector r) = Construction Maybe
	morphing (premorph -> Scalar x) = Construct x Nothing
	morphing (premorph -> Vector x xs) = item @Push x <-- into @(Nonempty List) xs
