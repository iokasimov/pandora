module Pandora.Paradigm.Inventory.Optics where

import Pandora.Core.Functor (type (:=>))
import Pandora.Pattern.Category ((.), ($))
import Pandora.Pattern.Functor.Covariant ((<$))
import Pandora.Pattern.Functor.Extractable (Extractable (extract))
import Pandora.Pattern.Functor.Representable (Representable (Representation, (<#>), tabulate))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Paradigm.Controlflow.Effect.Interpreted (run, unite)
import Pandora.Paradigm.Primary.Functor.Product (Product ((:*:)))
import Pandora.Paradigm.Primary.Object.Boolean ((?))
import Pandora.Paradigm.Inventory.Store (Store (Store), position, look, retrofit)
import Pandora.Paradigm.Schemes.PQ_ (PQ_ (PQ_))

infixr 0 :-.
infixr 0 :~.

type (:-.) src tgt = Lens src tgt

-- Reference to taret within some source
type Lens = PQ_ (->) Store

-- Lens as natural transformation
type (:~.) src tgt = forall a . Lens (src a) (tgt a)

-- | Lens composition infix operator
(|>) :: Lens src tgt -> Lens tgt new -> Lens src new
(|>) from to = PQ_ $ \src -> src <$ (run to . position $ run from src)

-- | Get the target of a lens
view :: Lens src tgt -> src -> tgt
view lens = position . run lens

-- | Replace the target of a lens
set :: Lens src tgt -> tgt -> src -> src
set lens new = look new . run lens

-- | Modify the target of a lens
over :: Lens src tgt -> (tgt -> tgt) -> src -> src
over lens f = extract . retrofit f . run lens

-- | Representable based lens
represent :: (Representable t, Setoid (Representation t)) => Representation t -> t a :-. a
represent r = PQ_ $ \x -> Store $ (r <#> x) :*: \new -> tabulate (\r' -> r' == r ? new $ r' <#> x)
