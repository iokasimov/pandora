module Pandora.Paradigm.Primary.Object.Natural where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))

data Natural = Zero | Natural Natural

instance Semigroup Natural where
	Zero + m = m
	Natural n + m = Natural $ n + m

instance Ringoid Natural where
	Zero * m = Zero
	Natural n * m = m + n * m

instance Monoid Natural where
	zero = Zero

instance Quasiring Natural where
	one = Natural Zero
