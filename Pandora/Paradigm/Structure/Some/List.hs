{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.List where

import Pandora.Core.Functor (type (<), type (>), type (>>>>>>))
import Pandora.Core.Impliable (imply)
import Pandora.Core.Interpreted (run, (<~), (<~~~), (=#-))
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((<--), (<---), (<----), (<-----), (-->), (--->), identity)
import Pandora.Pattern.Kernel (constant)
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((<-|-), (<-|--)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<-/-)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<), (==<<), (===<<)))
import Pandora.Pattern.Transformation.Liftable (lift)
import Pandora.Pattern.Transformation.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((==), (?=)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Algebraic ((<-*--), (-*), (-+), extract, point, empty, void)
import Pandora.Pattern.Operation.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Algebraic (type (<:*:>), (<:*:>))
import Pandora.Pattern.Operation.Exponential ((.:..), (%))
import Pandora.Paradigm.Primary.Auxiliary (Horizontal (Left, Right))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Exactly (Exactly (Exactly))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct)
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Primary ()
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Inventory.Ability.Gettable (get)
import Pandora.Paradigm.Inventory.Ability.Settable (set)
import Pandora.Paradigm.Inventory.Ability.Modifiable (modify)
import Pandora.Paradigm.Inventory.Some.State (State, current, change)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Structure.Modification.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Rotate, Into, Push, Pop, Delete, Find, Lookup, Element, Key)
	, Occurrence (All, First), premorph, item, filter, find, lookup, into)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Substance, substructure, sub), Segment (Root, Rest))
import Pandora.Paradigm.Structure.Interface.Stack (Stack (Topping, push, pop, top))
import Pandora.Paradigm.Structure.Interface.Zipper (Zippable (Breadcrumbs, fasten, unfasten), Zipper)
import Pandora.Paradigm.Structure.Modification.Combinative (Combinative)
import Pandora.Paradigm.Structure.Modification.Comprehension (Comprehension (Comprehension))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed)
import Pandora.Paradigm.Structure.Modification.Tape (Tape)
import Pandora.Paradigm.Structure.Modification.Turnover (Turnover (Turnover))

-- | Linear data structure that serves as a collection of elements
type List = Maybe <::> Construction Maybe

instance Setoid a => Setoid (List a) where
	TT ls == TT rs = ls == rs

instance Semigroup (List a) where
	TT Nothing + TT ys = TT ys
	TT (Just (Construct x xs)) + TT ys = lift . Construct x . run
		<-- TT @Covariant @Covariant xs + TT @Covariant @Covariant ys

instance Monoid (List a) where
	zero = empty

instance Morphable Push List where
	type Morphing Push List = Exactly <:.:> List >>>>>> (->)
	morphing (premorph -> xs) = T_U <-- lift . (Construct % run xs) . extract

instance Morphable Pop List where
	type Morphing Pop List = List
	morphing (premorph -> xs) = resolve deconstruct Nothing =#- xs

instance Morphable (Find Element) List where
	type Morphing (Find Element) List = Predicate <:.:> Maybe >>>>>> (->)
	morphing list = case run --> premorph list of
		Nothing -> T_U <-- \_ -> Nothing
		Just (Construct x xs) -> T_U <-- \p ->
			(p <~ x) ?= True <---- Just x
				<---- find @Element @List @Maybe <-- p <-- TT xs

instance Morphable (Delete First) List where
	type Morphing (Delete First) List = Predicate <:.:> List >>>>>> (->)
	morphing list = case run --> premorph list of
		Nothing -> T_U <-- constant empty
		Just (Construct x xs) -> T_U <-- \p ->
			(p <~ x) ?= True <--- TT xs
				<--- lift . Construct x . run . filter @First @List p <-- TT xs

instance Morphable (Delete All) List where
	type Morphing (Delete All) List = Predicate <:.:> List >>>>>> (->)
	morphing list = case run <--- premorph list of
		Nothing -> T_U <-- constant empty
		Just (Construct x xs) -> T_U <-- \p -> (p <~ x) ?= True
				<--- filter @All @List p <-- TT xs
				<--- lift . Construct x . run . filter @All @List p <-- TT xs

instance Stack List where
	type Topping List = Maybe
	top = P_Q_T <-- \list -> case list of
		TT Nothing -> Store <--- Nothing :*: constant empty
		TT (Just xs) -> Store <--- Just (extract xs) :*: \new -> case new of
			Nothing -> TT <-- deconstruct xs
			Just x -> TT <---- Construct x . Just <-|- deconstruct xs
	pop = resolve @(Nonempty List _) (\(Construct x xs) -> constant (Just x) <-|- set @State (TT xs)) (point Nothing) . run ==<< get @State
	push x = modify @State (item @Push x) -* point x

instance Substructure Root List where
	type Substance Root List = Maybe
	substructure = P_Q_T <-- \zipper -> case run --> lower zipper of
		Just (Construct x xs) -> Store <--- Just x :*: lift . resolve (lift . (Construct % xs)) zero
		Nothing -> Store <--- Nothing :*: lift . resolve (lift . point) zero

instance Substructure Rest List where
	type Substance Rest List = List
	substructure = P_Q_T <-- \source -> case run . lower <-- source of
		Just ns -> lift . lift @(->) <-|- run (sub @Rest) ns
		Nothing -> Store <--- zero :*: lift . identity

-- | Transform any traversable structure into a list
-- linearize :: forall t a . Traversable (->) (->) t => t a -> List a
-- linearize = TT . extract . (run @(->) @(State (Maybe :. Nonempty List >>> a)) % Nothing) . fold (Just .:.. Construct)

----------------------------------------- Non-empty list -------------------------------------------

instance {-# OVERLAPS #-} Semigroup (Construction Maybe a) where
	Construct x Nothing + ys = Construct x <-- Just ys
	Construct x (Just xs) + ys = Construct x . Just <-- xs + ys

instance Morphable (Find Element) (Construction Maybe) where
	type Morphing (Find Element) (Construction Maybe) = Predicate <:.:> Maybe >>>>>> (->)
	morphing (premorph -> Construct x xs) = T_U <-- \p -> (p <~ x) ?= True <----- Just x
		<----- find @Element @(Nonempty List) @Maybe <-- p ===<< xs

instance Morphable (Into List) (Construction Maybe) where
	type Morphing (Into List) (Construction Maybe) = List
	morphing = lift . premorph

instance Morphable (Into List) (Construction Maybe <::> Maybe) where
	type Morphing (Into List) (Construction Maybe <::> Maybe) = List
	morphing nonempty_list_with_maybe_elements = case run . premorph ---> nonempty_list_with_maybe_elements of
		Construct (Just x) (Just xs) -> item @Push x --> into @List --> TT @Covariant @Covariant xs
		Construct (Just x) Nothing -> point x
		Construct Nothing (Just xs) -> into @List --> TT @Covariant @Covariant xs
		Construct Nothing Nothing -> empty

instance Morphable Push (Construction Maybe) where
	type Morphing Push (Construction Maybe) = Exactly <:.:> Construction Maybe >>>>>> (->)
	morphing (premorph -> xs) = T_U <-- \(Exactly x) -> Construct x <-- Just xs

instance Stack (Construction Maybe) where
	type Topping (Construction Maybe) = Exactly
	top = P_Q_T <-- \xs -> Store <--- Exactly (extract xs) :*: \(Exactly new) -> Construct new <--- deconstruct xs
	-- It will never return you the last element
	pop = (\(Construct x xs) -> constant <-- Exactly x <-|-- change @(Nonempty List _) . constant <-/- xs) =<< current @(Nonempty List _)
	push x = (modify @State <-- Construct x . Just) -* point x

---------------------------------------- Combinative list ------------------------------------------

type instance Combinative List = Comprehension Maybe

----------------------------------------- Zipper of list -------------------------------------------

instance Zippable List where
	type Breadcrumbs List = Reverse List <:*:> List
	fasten (TT (Just (Construct x xs))) = Just <----- Exactly x <:*:> Reverse <-- TT empty <:*:> TT xs
	fasten (TT Nothing) = Nothing
	unfasten :: forall e . Zipper List e -> Nonempty List e
	unfasten (T_U (Exactly focus :*: T_U (Reverse left :*: right))) =
		attached <-- (push @(Nonempty List) <-/- left) <~ Construct focus (run right) where

-- TODO: No overlapping, let's use wrappers instead
instance {-# OVERLAPS #-} Traversable (->) (->) (Tape List) where
	f <-/- T_U (Exactly x :*: T_U (left :*: right)) =
		(\past' x' left' -> Exactly x' <:*:> left' <:*:> run past')
			<-|-- f <-/- Reverse right <-*-- f x <-*-- f <-/- left

-- TODO: Try to generalize to Extendable (Tape structure)
-- instance {-# OVERLAPS #-} Extendable (->) (Tape List) where
-- 	f <<= z = let move rtt = TT . deconstruct <----- run . rtt .-+ z in
-- 		imply @(Tape List _)
-- 			<---- f z
-- 			<---- f <-|-- move <-- rotate @Left
-- 			<---- f <-|-- move <-- rotate @Right

-- TODO: Define Stack structure => Slidable Left (Turnover < Tape structure)
instance Morphable (Rotate Left) (Turnover < Tape List) where
	type Morphing (Rotate Left) (Turnover < Tape List) = Turnover < Tape List
	morphing s@(run . premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) =
		resolve @(Tape List _) <--- Turnover <--- premorph s <----
			(rotate_left x right <-|- run left) -+ (rotate_over x <-|- run right) where

		rotate_left :: a -> List a -> Nonempty List a -> Tape List a
		rotate_left focused rs (Construct lx lxs) = imply @(Tape List _) <-- lx <-- TT lxs <-- item @Push focused rs

		rotate_over :: a -> Nonempty List a -> Tape List a
		rotate_over focused rs = let new_left = attached <--- (put_over <-/- rs <~~~ point focused) in
			imply @(Tape List _) <--- extract new_left <--- TT <-- deconstruct new_left <--- empty

		put_over :: a -> State < Nonempty List a < ()
		put_over = void . modify @State . item @Push

-- TODO: Define Stack structure => Slidable Right (Turnover < Tape structure)
instance Morphable (Rotate Right) (Turnover < Tape List) where
	type Morphing (Rotate Right) (Turnover < Tape List) = Turnover < Tape List
	morphing s@(run . premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) =
		resolve @(Tape List _) <--- Turnover <--- premorph s
			<---- (rotate_right x left <-|- run right) -+ (rotate_over x <-|- run left) where

		rotate_right :: a -> List a -> Nonempty List a -> Tape List a
		rotate_right focused ls (Construct rx rxs) = imply @(Tape List _) <-- rx <-- item @Push focused ls <-- TT rxs

		rotate_over :: a -> Nonempty List a -> Tape List a
		rotate_over focused ls = let new_right = attached (put_over <-/- ls <~~~ point focused) in
			imply @(Tape List _) <--- extract new_right <--- empty <--- TT <-- deconstruct new_right

		put_over :: a -> State (Nonempty List a) ()
		put_over = void . modify @State . item @Push

instance Morphable (Into > Tape List) List where
	type Morphing (Into > Tape List) List = Maybe <::> Tape List
	morphing (premorph -> list) = (into @(Zipper List) <-|-) =#- list

instance Morphable (Into List) (Tape List) where
	type Morphing (Into List) (Tape List) = List
	morphing (premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) = attached <----- run @(->) @(State _)
		<---- modify @State . item @Push @List <-/- right
		<---- item @Push x left

instance Morphable (Into > Comprehension Maybe) (Tape List) where
	type Morphing (Into > Comprehension Maybe) (Tape List) = Comprehension Maybe
	morphing (premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) = attached <----- run @(->) @(State _)
		<---- modify @State . item @Push @(Comprehension Maybe) <-/- right
		<---- item @Push x <-- Comprehension left

------------------------------------- Zipper of non-empty list -------------------------------------

instance Morphable (Rotate Left) (Tape > Construction Maybe) where
	type Morphing (Rotate Left) (Tape > Construction Maybe) = Maybe <::> (Tape > Construction Maybe)
	morphing (premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) =
		TT <----- imply @(Tape (Nonempty List) _)
			<-|-- point <-- extract left
			<-*-- deconstruct left
			<-*-- point <-- item @Push x right

instance Morphable (Rotate Right) (Tape > Construction Maybe) where
	type Morphing (Rotate Right) (Tape > Construction Maybe) = Maybe <::> Tape (Construction Maybe)
	morphing (premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) =
		TT <----- imply @(Tape < Nonempty List < _)
			<-|-- point <-- extract right
			<-*-- point <-- item @Push x left
			<-*-- deconstruct right

instance Morphable (Into > Tape List) (Construction Maybe) where
	type Morphing (Into > Tape List) (Construction Maybe) = Tape List
	morphing (premorph -> ne) = imply @(Tape List _) <--- extract ne <--- empty <--- TT <-- deconstruct ne

--instance Morphable (Into > Tape List) (Tape > Construction Maybe) where
	--type Morphing (Into > Tape List) (Tape > Construction Maybe) = Tape List
	--morphing (premorph -> zipper) = (((((((lift =#-) :*: lift) <-|-<-|-) =#-) <-|-) =#-) =#-) zipper

--instance Morphable (Into > Tape > Construction Maybe) (Tape List) where
	--type Morphing (Into > Tape > Construction Maybe) (Tape List) =
		--Maybe <::> Tape (Construction Maybe)
	--morphing (lower . premorph -> zipper) = let spread x y = (\x' y' -> Reverse x' :*: y') <-|- x <-*- y in
		--lift . TT <--- T_U . (Exactly <-- extract zipper :*:) . T_U <-|- ((spread |-) . ((run . run :*: run) <-|-<-|-) . run . extract <-- run zipper)

instance Morphable (Into > Construction Maybe) (Tape > Construction Maybe) where
	type Morphing (Into > Construction Maybe) (Tape > Construction Maybe) = Construction Maybe
	morphing (premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) = attached <----- run @(->) @(State _)
		<---- modify @State . item @Push @(Nonempty List) <-/- right
		<---- item @Push x left

instance Morphable (Into List) (Tape > Construction Maybe) where
	type Morphing (Into List) (Tape > Construction Maybe) = List
	morphing (premorph -> T_U (Exactly x :*: T_U (Reverse left :*: right))) = attached <----- run @(->) @(State _)
		<---- modify @State . item @Push @List <-/- right
		<---- item @Push x <-- lift left

------------------------------------ Zipper of combinative list ------------------------------------

-- TODO: add `fasten` and `unfasten` implementations
instance Zippable (Comprehension Maybe) where
	type Breadcrumbs (Comprehension Maybe) = Comprehension Maybe <:*:> Comprehension Maybe

----------------------------------------- Prefixed list --------------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed List key) where
	type Morphing (Lookup Key) (Prefixed List key) = (->) key <::> Maybe
	morphing (run . premorph -> list) = TT <-- \key -> lookup @Key key ===<< TT @Covariant @Covariant <-|- run list

------------------------------------ Prefixed non-empty list ---------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed < Construction Maybe < key) where
	type Morphing (Lookup Key) (Prefixed < Construction Maybe < key) = (->) key <::> Maybe
	morphing (run . premorph -> Construct x xs) = TT <-- \key -> extract <-|- search key where
		search key = key ?= attached x
			<----- Just x
			<----- find @Element <--- Predicate <-- (key ==) . attached ===<< xs
