{-# OPTIONS_GHC -fno-warn-orphans #-}

module Pandora.Paradigm.Structure.Some.List where

import Pandora.Core.Functor (type (:.), type (:=), type (:::))
import Pandora.Core.Impliable (imply)
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category (($), (#), identity)
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((-<$>-)))
import Pandora.Pattern.Functor.Extractable (extract)
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Bivariant ((<->))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((|-)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False), (?))
import Pandora.Paradigm.Primary.Object.Numerator (Numerator (Numerator))
import Pandora.Paradigm.Primary.Object.Denumerator (Denumerator (Single))
import Pandora.Paradigm.Primary.Algebraic ((-<*>-), (-.#..-))
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), attached, twosome)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Tap (Tap (Tap))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Inventory.State (State, fold, modify)
import Pandora.Paradigm.Inventory.Store (Store (Store))
import Pandora.Paradigm.Inventory.Optics (Convex, Lens, view)
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (||=))
import Pandora.Paradigm.Schemes.TU (TU (TU), type (<:.>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Zipper (Zipper)
import Pandora.Paradigm.Structure.Ability.Measurable (Measurable (Measural, measurement), Scale (Length), measure)
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Rotate, Into, Push, Pop, Delete, Find, Lookup, Element, Key)
	, Occurrence (All, First), premorph, rotate, item, filter, find, lookup, into)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure, sub), Segment (Root, Tail))
import Pandora.Paradigm.Structure.Interface.Stack (Stack)
import Pandora.Paradigm.Structure.Modification.Combinative (Combinative)
import Pandora.Paradigm.Structure.Modification.Comprehension (Comprehension (Comprehension))
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
	zero = TU Nothing

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

instance Measurable Length List where
	type Measural Length List a = Numerator
	measurement (run . extract -> Nothing) = zero
	measurement (run . extract -> Just xs) = Numerator $ measure @Length xs

instance Nullable List where
	null = Predicate $ \case { TU Nothing -> True ; _ -> False }

instance Substructure Root List where
	type Available Root List = Maybe
	type Substance Root List = Identity
	substructure = P_Q_T $ \zipper -> case run # lower zipper of
		Just (Construct x xs) -> Store $ Just (Identity x) :*: lift . resolve (lift . (Construct % xs) . extract @Identity) zero
		Nothing -> Store $ Nothing :*: lift . resolve (lift . (Construct % Nothing) . extract @Identity) zero

instance Substructure Tail List where
	type Available Tail List = Identity
	type Substance Tail List = List
	substructure = P_Q_T $ \x -> case run . extract . run $ x of
		Just ns -> lift . lift -<$>- run (sub @Tail) ns
		Nothing -> Store $ Identity zero :*: lift . identity . extract

-- | Transform any traversable structure into a stack
linearize :: forall t a . Traversable t (->) (->) => t a -> List a
linearize = TU . extract . (run @(State (Maybe :. Nonempty List := a)) % Nothing) . fold (Just -.#..- Construct)

----------------------------------------- Non-empty list -------------------------------------------

type instance Nonempty List = Construction Maybe

instance {-# OVERLAPS #-} Semigroup (Construction Maybe a) where
	Construct x Nothing + ys = Construct x $ Just ys
	Construct x (Just xs) + ys = Construct x . Just $ xs + ys

instance Morphable (Find Element) (Construction Maybe) where
	type Morphing (Find Element) (Construction Maybe) = Predicate <:.:> Maybe := (->)
	morphing (premorph -> Construct x xs) = T_U $ \p ->
		run p x ? Just x $ find @Element @(Nonempty List) @Maybe # p =<< xs

instance Morphable (Into List) (Construction Maybe) where
	type Morphing (Into List) (Construction Maybe) = List
	morphing = lift . premorph

instance Morphable Push (Construction Maybe) where
	type Morphing Push (Construction Maybe) = Identity <:.:> Construction Maybe := (->)
	morphing (premorph -> xs) = T_U $ \(Identity x) -> Construct x $ Just xs

instance Measurable Length (Construction Maybe) where
	type Measural Length (Construction Maybe) a = Denumerator
	measurement (deconstruct . extract -> Nothing) = Single
	measurement (deconstruct . extract -> Just xs) = Single + measure @Length xs

instance Substructure Root (Construction Maybe) where
	type Available Root (Construction Maybe) = Identity
	type Substance Root (Construction Maybe) = Identity
	substructure = imply @(Convex Lens _ _) (Identity . extract . lower)
		(\source target -> lift $ Construct # extract target # deconstruct (lower source))

instance Substructure Tail (Construction Maybe) where
	type Available Tail (Construction Maybe) = Identity
	type Substance Tail (Construction Maybe) = List
	substructure = imply @(Convex Lens _ _) (TU . deconstruct . lower)
		(\source target -> lift $ Construct # extract (lower source) # run target)

---------------------------------------- Combinative list ------------------------------------------

type instance Combinative List = Comprehension Maybe

----------------------------------------- Zipper of list -------------------------------------------

type instance Zipper List (Left ::: Right) = Tap (List <:.:> List := (:*:))

instance {-# OVERLAPS #-} Traversable (Tap (List <:.:> List := (:*:))) (->) (->) where
	f <<- Tap x (T_U (future :*: past)) = (\past' x' future' -> Tap x' $ twosome # future' # run past')
		-<$>- f <<- Reverse past -<*>- f x -<*>- f <<- future

instance {-# OVERLAPS #-} Extendable (Tap (List <:.:> List := (:*:))) (->) where
	f <<= z = let move rtt = TU . deconstruct $ run . rtt .-+ z in
		Tap # f z $ twosome # f -<$>- move (rotate @Left) # f -<$>- move (rotate @Right)

instance Morphable (Rotate Left) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Rotate Left) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Tap (List <:.:> List := (:*:))
	morphing (premorph -> Tap x (T_U (future :*: past))) =
		let subtree = twosome # extract (view (sub @Tail) future) # item @Push x past in
		TU $ (Tap . extract) % subtree -<$>- view (sub @Root) future

instance Morphable (Rotate Right) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Rotate Right) (Tap (List <:.:> List := (:*:))) = Maybe <:.> Tap (List <:.:> List := (:*:))
	morphing (premorph -> Tap x (T_U (future :*: past))) =
		let subtree = twosome # item @Push x future # extract (view (sub @Tail) past) in
		TU $ (Tap . extract) % subtree -<$>- view (sub @Root) past

instance Morphable (Into (Tap (List <:.:> List := (:*:)))) List where
	type Morphing (Into (Tap (List <:.:> List := (:*:)))) List = Maybe <:.> Tap (List <:.:> List := (:*:))
	morphing (premorph -> list) = (into @(Zipper List (Left ::: Right)) -<$>-) ||= list

instance Morphable (Into List) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Into List) (Tap (List <:.:> List := (:*:))) = List
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# modify . item @Push @List <<- past
		# item @Push x future

instance Morphable (Into (Comprehension Maybe)) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Into (Comprehension Maybe)) (Tap (List <:.:> List := (:*:))) = Comprehension Maybe
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# modify . item @Push @(Comprehension Maybe) <<- past
		# item @Push x (Comprehension future)

------------------------------------- Zipper of non-empty list -------------------------------------

type instance Zipper (Construction Maybe) (Left ::: Right) = Tap (Construction Maybe <:.:> Construction Maybe := (:*:))

instance Morphable (Rotate Left) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Rotate Left) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) =
		Maybe <:.> Tap (Construction Maybe <:.:> Construction Maybe := (:*:))
	morphing (premorph -> Tap x (T_U (future :*: past))) = TU $ Tap (extract future) . twosome % item @Push x past -<$>- deconstruct future

instance Morphable (Rotate Right) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Rotate Right) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) =
		Maybe <:.> Tap (Construction Maybe <:.:> Construction Maybe := (:*:))
	morphing (premorph -> Tap x (T_U (future :*: past))) = TU $ Tap (extract past) . twosome (item @Push x future) -<$>- deconstruct past

instance Morphable (Into (Tap (List <:.:> List := (:*:)))) (Construction Maybe) where
	type Morphing (Into (Tap (List <:.:> List := (:*:)))) (Construction Maybe) = Tap (List <:.:> List := (:*:))
	morphing (premorph -> ne) = Tap # extract ne $ twosome # extract (view # sub @Tail # ne) # zero

instance Morphable (Into (Tap (List <:.:> List := (:*:)))) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Into (Tap (List <:.:> List := (:*:)))) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Tap (List <:.:> List := (:*:))
	morphing (premorph -> zipper) = Tap # extract zipper $ lift <-> lift ||= lower zipper

instance Morphable (Into (Tap (Construction Maybe <:.:> Construction Maybe := (:*:)))) (Tap (List <:.:> List := (:*:))) where
	type Morphing (Into (Tap (Construction Maybe <:.:> Construction Maybe := (:*:)))) (Tap (List <:.:> List := (:*:))) =
		Maybe <:.> Tap (Construction Maybe <:.:> Construction Maybe := (:*:))
	morphing (premorph -> zipper) = let spread x y = (:*:) -<$>- x -<*>- y in TU $
		Tap (extract zipper) . T_U -<$>- ((spread |-) . (run <-> run) . run $ lower zipper)

instance Morphable (Into (Construction Maybe)) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Into (Construction Maybe)) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = Construction Maybe
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# modify . item @Push @(Nonempty List) <<- past
		# item @Push x future

instance Morphable (Into List) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) where
	type Morphing (Into List) (Tap (Construction Maybe <:.:> Construction Maybe := (:*:))) = List
	morphing (premorph -> Tap x (T_U (future :*: past))) = attached $ run @(State _)
		# modify . item @Push @List <<- past
		# item @Push x (lift future)

------------------------------------ Zipper of combinative list ------------------------------------

type instance Zipper (Comprehension Maybe) (Left ::: Right) = Tap (Comprehension Maybe <:.:> Comprehension Maybe := (:*:))

----------------------------------------- Prefixed list --------------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed List key) where
	type Morphing (Lookup Key) (Prefixed List key) = (->) key <:.> Maybe
	morphing (run . premorph -> list) = TU $ \key -> lookup @Key key =<< Prefixed -<$>- run list 

------------------------------------ Prefixed non-empty list ---------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed (Construction Maybe) key) where
	type Morphing (Lookup Key) (Prefixed (Construction Maybe) key) = (->) key <:.> Maybe
	morphing (run . premorph -> Construct x xs) = TU $ \key -> extract @_ @(->) -<$>- search key where
		search key = key == attached x ? Just x $ find @Element # Predicate ((key ==) . attached) =<< xs
