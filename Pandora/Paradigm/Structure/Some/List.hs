{-# OPTIONS_GHC -fno-warn-orphans #-}
module Pandora.Paradigm.Structure.Some.List where

import Pandora.Core.Functor (type (:.), type (:=))
import Pandora.Core.Impliable (imply)
import Pandora.Pattern.Semigroupoid ((.))
import Pandora.Pattern.Category ((#), identity)
import Pandora.Pattern.Functor.Covariant (Covariant, Covariant ((<-|-)))
import Pandora.Pattern.Functor.Traversable (Traversable ((<<-)))
import Pandora.Pattern.Functor.Extendable (Extendable ((<<=)))
import Pandora.Pattern.Functor.Bindable (Bindable ((=<<)))
import Pandora.Pattern.Functor.Adjoint (Adjoint ((|-)))
import Pandora.Pattern.Transformer.Liftable (lift)
import Pandora.Pattern.Transformer.Lowerable (lower)
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Algebraic ((<-*-), (.-+-), (-.#..-), extract, point, empty, void)
import Pandora.Paradigm.Primary.Algebraic.Product ((:*:) ((:*:)), attached)
import Pandora.Paradigm.Primary.Algebraic.Exponential ((%))
import Pandora.Paradigm.Primary.Algebraic ((<-|-<-|-))
import Pandora.Paradigm.Primary.Functor.Maybe (Maybe (Just, Nothing))
import Pandora.Paradigm.Primary.Functor.Identity (Identity (Identity))
import Pandora.Paradigm.Primary.Functor.Predicate (Predicate (Predicate))
import Pandora.Paradigm.Primary.Functor.Wye (Wye (Left, Right))
import Pandora.Paradigm.Primary.Transformer.Construction (Construction (Construct), deconstruct, (.-+))
import Pandora.Paradigm.Primary.Transformer.Reverse (Reverse (Reverse))
import Pandora.Paradigm.Primary (twosome)
import Pandora.Paradigm.Inventory.Ability.Viewable (view)
import Pandora.Paradigm.Inventory.Ability.Modifiable (Modifiable (Modification, modify))
import Pandora.Paradigm.Inventory.Some.State (State, fold)
import Pandora.Paradigm.Inventory.Some.Store (Store (Store))
import Pandora.Paradigm.Inventory.Some.Optics (Convex, Obscure, Lens)
import Pandora.Paradigm.Controlflow.Effect.Conditional (Conditional ((?)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, (!), (||=))
import Pandora.Paradigm.Schemes.TT (TT (TT), type (<::>))
import Pandora.Paradigm.Schemes.T_U (T_U (T_U), type (<:.:>))
import Pandora.Paradigm.Schemes.P_Q_T (P_Q_T (P_Q_T))
import Pandora.Paradigm.Structure.Ability.Nonempty (Nonempty)
import Pandora.Paradigm.Structure.Ability.Nullable (Nullable (null))
import Pandora.Paradigm.Structure.Ability.Zipper (Zippable (Breadcrumbs), Zipper, Tape)
import Pandora.Paradigm.Structure.Ability.Monotonic (resolve)
import Pandora.Paradigm.Structure.Ability.Morphable (Morphable (Morphing, morphing)
	, Morph (Rotate, Into, Push, Pop, Delete, Find, Lookup, Element, Key)
	, Occurrence (All, First), premorph, rotate, item, filter, find, lookup, into)
import Pandora.Paradigm.Structure.Ability.Substructure (Substructure (Available, Substance, substructure, sub), Segment (Root, Tail))
import Pandora.Paradigm.Structure.Interface.Stack (Stack)
import Pandora.Paradigm.Structure.Modification.Combinative (Combinative)
import Pandora.Paradigm.Structure.Modification.Comprehension (Comprehension (Comprehension))
import Pandora.Paradigm.Structure.Modification.Prefixed (Prefixed (Prefixed))
import Pandora.Paradigm.Structure.Modification.Turnover (Turnover (Turnover))

-- | Linear data structure that serves as a collection of elements
type List = Maybe <::> Construction Maybe

instance Setoid a => Setoid (List a) where
	TT ls == TT rs = ls == rs

instance Semigroup (List a) where
	TT Nothing + TT ys = TT ys
	TT (Just (Construct x xs)) + TT ys = lift . Construct x . run
		! TT @Covariant @Covariant xs + TT @Covariant @Covariant ys

instance Monoid (List a) where
	zero = empty

instance Morphable Push List where
	type Morphing Push List = Identity <:.:> List := (->)
	morphing (premorph -> xs) = T_U ! lift . (Construct % run xs) . extract

instance Morphable Pop List where
	type Morphing Pop List = List
	morphing (premorph -> xs) = resolve deconstruct Nothing ||= xs

instance Morphable (Find Element) List where
	type Morphing (Find Element) List = Predicate <:.:> Maybe := (->)
	morphing list = case run # premorph list of
		Nothing -> T_U ! \_ -> Nothing
		Just (Construct x xs) -> T_U ! \p -> run p x ? Just x
			! find @Element @List @Maybe # p # TT xs

instance Morphable (Delete First) List where
	type Morphing (Delete First) List = Predicate <:.:> List := (->)
	morphing list = case run # premorph list of
		Nothing -> T_U ! \_ -> empty
		Just (Construct x xs) -> T_U ! \p -> 
			run p x ? TT xs ! lift . Construct x . run . filter @First @List p # TT xs

instance Morphable (Delete All) List where
	type Morphing (Delete All) List = Predicate <:.:> List := (->)
	morphing list = case run # premorph list of
		Nothing -> T_U ! \_ -> empty
		Just (Construct x xs) -> T_U ! \p ->
			run p x ? filter @All @List p (TT xs)
				! lift . Construct x . run . filter @All @List p # TT xs

instance Stack List where

instance Nullable List where
	null = Predicate ! \case { TT Nothing -> True ; _ -> False }

instance Substructure Root List where
	type Available Root List = Maybe
	type Substance Root List = Identity
	substructure = P_Q_T ! \zipper -> case run # lower zipper of
		Just (Construct x xs) -> Store ! Just (Identity x) :*: lift . resolve (lift . (Construct % xs) . extract @Identity) zero
		Nothing -> Store ! Nothing :*: lift . resolve (lift . (Construct % Nothing) . extract @Identity) zero

instance Substructure Tail List where
	type Available Tail List = Identity
	type Substance Tail List = List
	substructure = P_Q_T ! \x -> case run . extract . run ! x of
		Just ns -> lift . lift @(->) <-|- run (sub @Tail) ns
		Nothing -> Store ! Identity zero :*: lift . identity . extract

-- | Transform any traversable structure into a stack
linearize :: forall t a . Traversable (->) (->) t => t a -> List a
linearize = TT . extract . (run @(->) @(State (Maybe :. Nonempty List := a)) % Nothing) . fold (Just -.#..- Construct)

----------------------------------------- Non-empty list -------------------------------------------

type instance Nonempty List = Construction Maybe

instance {-# OVERLAPS #-} Semigroup (Construction Maybe a) where
	Construct x Nothing + ys = Construct x ! Just ys
	Construct x (Just xs) + ys = Construct x . Just ! xs + ys

instance Morphable (Find Element) (Construction Maybe) where
	type Morphing (Find Element) (Construction Maybe) = Predicate <:.:> Maybe := (->)
	morphing (premorph -> Construct x xs) = T_U ! \p ->
		run p x ? Just x ! find @Element @(Nonempty List) @Maybe # p =<< xs

instance Morphable (Into List) (Construction Maybe) where
	type Morphing (Into List) (Construction Maybe) = List
	morphing = lift . premorph

instance Morphable (Into List) (Construction Maybe <::> Maybe) where
	type Morphing (Into List) (Construction Maybe <::> Maybe) = List
	morphing nonempty_list_with_maybe_elements = case run . premorph # nonempty_list_with_maybe_elements of
		Construct (Just x) (Just xs) -> item @Push x # into @List (TT @Covariant @Covariant xs)
		Construct (Just x) Nothing -> point x
		Construct Nothing (Just xs) -> into @List (TT @Covariant @Covariant xs)
		Construct Nothing Nothing -> empty

instance Morphable Push (Construction Maybe) where
	type Morphing Push (Construction Maybe) = Identity <:.:> Construction Maybe := (->)
	morphing (premorph -> xs) = T_U ! \(Identity x) -> Construct x ! Just xs

instance Substructure Root (Construction Maybe) where
	type Available Root (Construction Maybe) = Identity
	type Substance Root (Construction Maybe) = Identity
	substructure = imply @(Convex Lens _ _) (Identity . extract . lower)
		(\source target -> lift (Construct # extract target # deconstruct (lower source)))

instance Substructure Tail (Construction Maybe) where
	type Available Tail (Construction Maybe) = Identity
	type Substance Tail (Construction Maybe) = List
	substructure = imply @(Convex Lens _ _) (TT . deconstruct . lower)
		(\source target -> lift (Construct # extract (lower source) # run target))

---------------------------------------- Combinative list ------------------------------------------

type instance Combinative List = Comprehension Maybe

----------------------------------------- Zipper of list -------------------------------------------

instance Zippable List where
	type Breadcrumbs List = (Reverse List <:.:> List := (:*:))

instance {-# OVERLAPS #-} Traversable (->) (->) (Tape List) where
	f <<- T_U (Identity x :*: T_U (left :*: right)) = (\past' x' left' -> twosome (Identity x') ! twosome # left' # run past')
		<-|- f <<- Reverse right <-*- f x <-*- f <<- left

instance {-# OVERLAPS #-} Extendable (->) (Tape List) where
	f <<= z = let move rtt = TT . deconstruct ! run . rtt .-+ z in
		twosome (Identity # f z) ! twosome # Reverse (f <-|- move (rotate @Left)) # f <-|- move (rotate @Right)

instance Morphable (Rotate Left) (Tape List) where
	type Morphing (Rotate Left) (Tape List) = Maybe <::> Tape List
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) =
		let subtree = twosome # Reverse (view @(Convex Lens) # sub @Tail # left) # item @Push x right in
		TT ! (twosome . Identity . extract) % subtree <-|- view @(Obscure Lens) (sub @Root) left

instance Morphable (Rotate Right) (Tape List) where
	type Morphing (Rotate Right) (Tape List) = Maybe <::> Tape List
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) =
		let subtree = twosome # Reverse (item @Push x left) # view @(Convex Lens) (sub @Tail) right in
		TT ! twosome % subtree <-|- view @(Obscure Lens) (sub @Root) right

instance Morphable (Rotate Left) (Turnover (Tape List)) where
	type Morphing (Rotate Left) (Turnover (Tape List)) = Turnover (Tape List)
	morphing s@(premorph -> Turnover (T_U (Identity x :*: T_U (Reverse left :*: right)))) =
		resolve @(Tape List _) Turnover # premorph s ! (rotate_over x <-|- run right) .-+- (rotate_left x right <-|- run left) where

		rotate_left :: a -> List a -> Nonempty List a -> Tape List a
		rotate_left focused rs (Construct lx lxs) = twosome # point lx
			! twosome # Reverse (TT lxs) # item @Push focused rs

		rotate_over :: a -> Nonempty List a -> Tape List a
		rotate_over focused rs = let new_left = attached (put_over <<- rs ! point focused) in
			twosome # point (extract new_left) ! twosome (Reverse . TT # deconstruct new_left) empty

		put_over :: a -> State (Nonempty List a) ()
		put_over = void . modify @State . item @Push

instance Morphable (Rotate Right) (Turnover (Tape List)) where
	type Morphing (Rotate Right) (Turnover (Tape List)) = Turnover (Tape List)
	morphing s@(premorph -> Turnover (T_U (Identity x :*: T_U (Reverse left :*: right)))) =
		resolve @(Tape List _) Turnover # premorph s ! (rotate_over x <-|- run left) .-+- (rotate_right x left <-|- run right) where

		rotate_right :: a -> List a -> Nonempty List a -> Tape List a
		rotate_right focused ls (Construct rx rxs) = twosome # point rx
			! twosome # Reverse (item @Push focused ls) # TT rxs

		rotate_over :: a -> Nonempty List a -> Tape List a
		rotate_over focused ls = let new_right = attached (put_over <<- ls ! point focused) in
			twosome # point (extract new_right) ! twosome (Reverse empty) (TT # deconstruct new_right)

		put_over :: a -> State (Nonempty List a) ()
		put_over = void . modify @State . item @Push

instance Morphable (Into (Tape List)) List where
	type Morphing (Into (Tape List)) List = Maybe <::> Tape List
	morphing (premorph -> list) = (into @(Zipper List) <-|-) ||= list

instance Morphable (Into List) (Tape List) where
	type Morphing (Into List) (Tape List) = List
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) = attached ! run @(->) @(State _)
		# modify @State . item @Push @List <<- right
		# item @Push x left

instance Morphable (Into (Comprehension Maybe)) (Tape List) where
	type Morphing (Into (Comprehension Maybe)) (Tape List) = Comprehension Maybe
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) = attached ! run @(->) @(State _)
		# modify @State . item @Push @(Comprehension Maybe) <<- right
		# item @Push x (Comprehension left)

------------------------------------- Zipper of non-empty list -------------------------------------

instance Zippable (Construction Maybe) where
	type Breadcrumbs (Construction Maybe) = Reverse (Construction Maybe) <:.:> Construction Maybe := (:*:)

instance Morphable (Rotate Left) (Tape (Construction Maybe)) where
	type Morphing (Rotate Left) (Tape (Construction Maybe)) =
		Maybe <::> (Tape (Construction Maybe))
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) =
		TT ! T_U . (Identity (extract left) :*:) . (twosome % item @Push x right) . Reverse <-|- deconstruct left

instance Morphable (Rotate Right) (Tape (Construction Maybe)) where
	type Morphing (Rotate Right) (Tape (Construction Maybe)) =
		Maybe <::> Tape (Construction Maybe)
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) =
		TT ! T_U . (Identity (extract right) :*:) . twosome (Reverse # item @Push x left) <-|- deconstruct right

instance Morphable (Into (Tape List)) (Construction Maybe) where
	type Morphing (Into (Tape List)) (Construction Maybe) = Tape List
	morphing (premorph -> ne) = twosome # Identity (extract ne) ! twosome # Reverse zero # (view @(Convex Lens) # sub @Tail # ne)

instance Morphable (Into (Tape List)) (Tape (Construction Maybe)) where
	type Morphing (Into (Tape List)) (Tape (Construction Maybe)) = Tape List
	morphing (premorph -> zipper) = ((((lift ||=) :*: lift <-|-<-|-) ||=) <-|-) ||= zipper

instance Morphable (Into (Tape (Construction Maybe))) (Tape List) where
	type Morphing (Into (Tape (Construction Maybe))) (Tape List) =
		Maybe <::> Tape (Construction Maybe)
	morphing (premorph -> zipper) = let spread x y = (\x' y' -> Reverse x' :*: y') <-|- x <-*- y in
		TT ! T_U . (Identity (extract zipper) :*:) . T_U <-|- ((spread |-) . (run . run :*: run <-|-<-|-) . run . extract ! run zipper)

instance Morphable (Into (Construction Maybe)) (Tape (Construction Maybe)) where
	type Morphing (Into (Construction Maybe)) (Tape (Construction Maybe)) = Construction Maybe
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) = attached ! run @(->) @(State _)
		# modify @State . item @Push @(Nonempty List) <<- right
		# item @Push x left

instance Morphable (Into List) (Tape (Construction Maybe)) where
	type Morphing (Into List) (Tape (Construction Maybe)) = List
	morphing (premorph -> T_U (Identity x :*: T_U (Reverse left :*: right))) = attached ! run @(->) @(State _)
		# modify @State . item @Push @List <<- right
		# item @Push x (lift left)

------------------------------------ Zipper of combinative list ------------------------------------

instance Zippable (Comprehension Maybe) where
	type Breadcrumbs (Comprehension Maybe) = (Comprehension Maybe <:.:> Comprehension Maybe := (:*:))

----------------------------------------- Prefixed list --------------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed List key) where
	type Morphing (Lookup Key) (Prefixed List key) = (->) key <::> Maybe
	morphing (run . premorph -> list) = TT ! \key -> lookup @Key key =<< Prefixed <-|- run list

------------------------------------ Prefixed non-empty list ---------------------------------------

instance Setoid key => Morphable (Lookup Key) (Prefixed (Construction Maybe) key) where
	type Morphing (Lookup Key) (Prefixed (Construction Maybe) key) = (->) key <::> Maybe
	morphing (run . premorph -> Construct x xs) = TT ! \key -> extract <-|- search key where
		search key = key == attached x ? Just x ! find @Element # Predicate ((key ==) . attached) =<< xs
