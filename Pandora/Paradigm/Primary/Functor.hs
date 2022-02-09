module Pandora.Paradigm.Primary.Functor (module Exports, Equivalence, Comparison) where

import Pandora.Paradigm.Primary.Functor.Fix as Exports
import Pandora.Paradigm.Primary.Functor.Convergence as Exports
import Pandora.Paradigm.Primary.Functor.Predicate as Exports
import Pandora.Paradigm.Primary.Functor.These as Exports
import Pandora.Paradigm.Primary.Functor.Validation as Exports
import Pandora.Paradigm.Primary.Functor.Wedge as Exports
import Pandora.Paradigm.Primary.Functor.Wye as Exports
import Pandora.Paradigm.Primary.Functor.Edges as Exports
import Pandora.Paradigm.Primary.Functor.Conclusion as Exports
import Pandora.Paradigm.Primary.Functor.Maybe as Exports
import Pandora.Paradigm.Primary.Functor.Endo as Exports
import Pandora.Paradigm.Primary.Functor.Proxy as Exports
import Pandora.Paradigm.Primary.Functor.Tagged as Exports
import Pandora.Paradigm.Primary.Functor.Constant as Exports
import Pandora.Paradigm.Primary.Functor.Exactly as Exports

import Pandora.Paradigm.Primary.Object.Boolean (Boolean)
import Pandora.Paradigm.Primary.Object.Ordering (Ordering)

type Equivalence = Convergence Boolean
type Comparison = Convergence Ordering
