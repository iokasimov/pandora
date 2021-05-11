{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.List where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Pattern.Category ((.), ($), (#), identity)
import Pandora.Pattern.Functor.Covariant (Covariant ((<$>), (.#..)))
import Pandora.Pattern.Functor.Applicative (Applicative ((<*>)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Avoidable (empty)
import Pandora.Pattern.Functor.Traversable (Traversable ((->>)))
import Pandora.Pattern.Functor.Extendable (Extendable ((=>>)))
import Pandora.Pattern.Functor.Bindable ((>>=))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Functor.Adjoint ((|-))
import Pandora.Pattern.Functor ()
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), (?))
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Numerator))
import Pandora.Paradigm.Primary.Object.Denumerator (Denumerator (One))
import Pandora.Paradigm.Primary.Functor.Function ((%), (&))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)), type (:*:), attached, twosome)
import Pandora.Paradigm.Primary.Functor.Tagged (Tagged (Tag))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Inventory.State (State, fold, modify)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (view)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Ability.Focusable (Focusable (Focusing, focusing), Location (Head), focus)
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Length), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (Monotonic (reduce, resolve))
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Rotate, Into, Push, Pop, Delete, Find, Lookup, Element, Key)
	, Occurrence (All, First), premorph, rotate, item, filter, find, lookup, into)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substructural, substructure, sub), Segment (Tail))
import Pandora.Paradigm.Structure.Interface.Stack (Stack)
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed (Prefixed))

-- | Linear data structure that serves as a collection of elements
type List = Maybe <:.> Construction Maybe

instance Setoid a => Setoid (List a) where
	TU ls == TU rs = ls == rs

instance Semigroup (List a) where
	TU Nothing + TU ys = TU ys
	TU (Just (Construct x xs)) + TU ys = lift . Construct x . run
		$ TU @Covariant @Covariant xs + TU @Covariant @Covariant ys

instance Monoid (List a) where
	zero = empty

instance Morphable Push List where
	type Morphing Push List = Identity <:.:> List := (->)
	morphing (premorph -> xs) = T_U $ lift . (Construct % run xs) . extract

instance Morphable Pop List where
	type Morphing Pop List = List
	morphing (premorph -> xs) = resolve deconstruct Nothing ||= xs

instance Morphable (Find Element) List where
	type Morphing (Find Element) List = Predicate <:.:> Maybe := (->)
	morphing (premorph -> TU Nothing) = T_U $ \_ -> Nothing
	morphing (premorph -> TU (Just (Construct x xs))) = T_U $ \p ->
		run p x ? Just x $ find @Element @List @Maybe # p # TU xs

instance Morphable (Delete First) List where
	type Morphing (Delete First) List = Predicate <:.:> List := (->)
	morphing (premorph -> TU Nothing) = T_U $ \_ -> TU Nothing
	morphing (premorph -> TU (Just (Construct x xs))) = T_U $ \p ->
		run p x ? TU xs $ lift . Construct x . run . filter @First @List p $ TU xs

instance Morphable (Delete All) List where
	type Morphing (Delete All) List = Predicate <:.:> List := (->)
	morphing (premorph -> TU Nothing) = T_U $ \_ -> TU Nothing
	morphing (premorph -> TU (Just (Construct x xs))) = T_U $ \p ->
		run p x ? filter @All @List p (TU xs) $ lift . Construct x . run . filter @All @List p $ TU xs

instance Stack List where

instance Focusable Head List where
	type Focusing Head List a = Maybe a
	focusing = PQ_ $ \stack -> Store $ extract <$> run (extract stack) :*: \case
		Just x -> extract stack & view (sub @Tail) & item @Push x & Tag
		Nothing -> extract stack & view (sub @Tail) & Tag

instance Measurable Length List where
	type Measural Length List a = Numerator
	measurement (run . extract -> Nothing) = zero
	measurement (run . extract -> Just xs) = Numerator $ measure @Length xs

instance Nullable List where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Tail List where
	type Substructural Tail List = List
	substructure = PQ_ $ \x -> case run . extract . run $ x of
		Just ns -> lift . lift <$> run (sub @Tail) ns
		Nothing -> Store $ empty :*: lift . identity

-- | Transform any traversable structure into a stack
linearize :: forall t a . Traversable t => t a -> List a
linearize = TU . extract . (run @(State (Maybe :. Nonempty List := a)) % Nothing) . fold (Just .#.. Construct)

----------------------------------------- Non-empty list -------------------------------------------

type instance Nonempty List = Construction Maybe

instance {-# OVERLAPS #-} Semigroup (Construction Maybe a) where
	Construct x Nothing + ys = Construct x $ Just ys
	Construct x (Just xs) + ys = Construct x . Just $ xs + ys

instance Morphable (Find Element) (Construction Maybe) where
	type Morphing (Find Element) (Construction Maybe) = Predicate <:.:> Maybe := (->)
	morphing (premorph -> Construct x xs) = T_U $ \p ->
		run p x ? Just x $ xs >>= find @Element @(Nonempty List) @Maybe # p

instance Morphable (Into List) (Construction Maybe) where
	type Morphing (Into List) (Construction Maybe) = List
	morphing = lift . premorph

instance Focusable Head (Construction Maybe) where
	type Focusing Head (Construction Maybe) a = a
	focusing = PQ_ $ \stack -> Store $ extract (extract stack) :*: Tag . Construct % deconstruct (extract stack)

instance Morphable Push (Construction Maybe) where
	type Morphing Push (Construction Maybe) = Identity <:.:> Construction Maybe := (->)
	morphing (premorph -> xs) = T_U $ \(Identity x) -> Construct x $ Just xs

instance Measurable Length (Construction Maybe) where
	type Measural Length (Construction Maybe) a = Denumerator
	measurement (deconstruct . extract -> Nothing) = One
	measurement (deconstruct . extract -> Just xs) = One + measure @Length xs

instance Substructure Tail (Construction Maybe) where
	type Substructural Tail (Construction Maybe) = List
	substructure = PQ_ $ \stack -> case extract $ run stack of
		Construct x xs -> Store $ TU xs :*: lift . Construct x . run

----------------------------------------- Zipper of list -------------------------------------------

type instance Zipper List = Tap (List <:.:> List := (:*:))

instance {-# OVERLAPS #-} Traversable (Tap (List <:.:> List := (:*:))) where
	Tap x (T_U (future :*: past)) ->> f = (\past' x' future' -> Tap x' $ twosome # future' # run past')
		<$> Reverse past ->> f <*> f x <*> future ->> f

instance {-# OVERLAPS #-} Extendable (Tap (List <:.:> List := (:*:))) where
	z =>> f = let move rtt = TU . deconstruct $ run . rtt .-+ z in
		Tap # f z $ twosome # f <$> move (rotate @Left) # f <$> move (rotate @Right)

instance Focusable Head (Tap (List <:.:> List := (:*:))) where
	type Focusing Head (Tap (List <:.:> List := (:*:))) a = a
	focusing = PQ_ $ \zipper -> Store $ extract (extract zipper) :*: Tag . Tap % lower (extract zipper)

instance Morphable (Rotate Left) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Rotate Left) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Zipper List
	morphing (premorph -> Tap x (T_U (future :*: past))) = TU
		$ Tap % twosome (view (sub @Tail) future) (item @Push x past) <$> view (focus @Head) future

instance Morphable (Rotate Right) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Rotate Right) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Zipper List
	morphing (premorph -> Tap x (T_U (future :*: past))) = TU
		$ Tap % twosome (item @Push x future) (view (sub @Tail) past) <$> view (focus @Head) past

instance Morphable (Into (Tap (List <:.:> List := (:*:)))) List where
	type Morphing (Into (Tap (List <:.:> List := (:*:)))) List = Maybe <:.> Zipper List
	morphing (premorph -> list) = (into @(Zipper List) <$>) ||= list

instance Morphable (Into List) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Into List) (Tap (List <:.:> List := (:*:))) = List
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# past ->> modify . item @Push @List
		# item @Push x future

------------------------------------- Zipper of non-empty list -------------------------------------

type instance Zipper (Construction Maybe) = Tap (Construction Maybe <:.:> Construction Maybe := (:*:))

instance {-# OVERLAPS #-} Traversable (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	Tap x (T_U (future :*: past)) ->> f = (\past' x' future' -> Tap x' $ twosome # future' # run past')
		<$> Reverse past ->> f <*> f x <*> future ->> f

instance Focusable Head (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Focusing Head (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) a = a
	focusing = PQ_ $ \zipper -> Store $ extract (extract zipper) :*: Tag . Tap % lower (extract zipper)

instance Morphable (Rotate Left) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Rotate Left) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Maybe <:.> Zipper (Construction Maybe)
	morphing (premorph -> Tap x (T_U (future :*: past))) = TU $ Tap (extract future) . twosome % item @Push x past <$> deconstruct future

instance Morphable (Rotate Right) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Rotate Right) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Maybe <:.> Zipper (Construction Maybe)
	morphing (premorph -> Tap x (T_U (future :*: past))) = TU $ Tap (extract past) . twosome (item @Push x future) <$> deconstruct past

instance Morphable (Into (Tap (List <:.:> List := (:*:)))) (Construction Maybe) where
	type Morphing (Into (Tap (List <:.:> List := (:*:)))) (Construction Maybe) = Zipper List
	morphing (premorph -> ne) = Tap # extract ne $ twosome # view (sub @Tail) ne # empty

instance Morphable (Into (Tap (List <:.:> List := (:*:)))) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Into (Tap (List <:.:> List := (:*:)))) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Zipper List
	morphing (premorph -> zipper) = Tap # extract zipper $ lift <-> lift ||= lower zipper

instance Morphable (Into (Tap (Construction Maybe <:.:> Construction Maybe := (:*:)))) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Into (Tap (Construction Maybe <:.:> Construction Maybe := (:*:)))) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Zipper (Construction Maybe)
	morphing (premorph -> zipper) = let spread x y = (:*:) <$> x <*> y in TU $
		Tap (extract zipper) . T_U <$> ((|- spread) . (run <-> run) . run $ lower zipper)

instance Morphable (Into (Construction Maybe)) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Into (Construction Maybe)) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Construction Maybe
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# past ->> modify . item @Push @(Nonempty List)
		# item @Push x future

instance Morphable (Into List) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Into List) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = List
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# past ->> modify . item @Push @List
		# item @Push x (lift future)

instance Monotonic a (Maybe <:.> Construction Maybe := a) where
	reduce f r = reduce f r . run

----------------------------------------- Prefixed list --------------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed List key) where
	type Morphing (Lookup Key) (Prefixed List key) = (->) key <:.> Maybe
	morphing (run . premorph -> list) = TU $ \key -> Prefixed <$> run list >>= lookup @Key key

------------------------------------ Prefixed non-empty list ---------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed (Construction Maybe) key) where
	type Morphing (Lookup Key) (Prefixed (Construction Maybe) key) = (->) key <:.> Maybe
	morphing (run . premorph -> Construct x xs) = TU $ \key -> extract <$> search key where
		search key = key == attached x ? Just x $ xs >>= find @Element # Predicate ((key ==) . attached)
