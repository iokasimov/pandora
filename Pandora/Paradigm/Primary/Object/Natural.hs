module Pandora.Paradigm.Primary.Object.Natural where

import Pandora.Pattern.Category (($))
import Pandora.Pattern.Object.Setoid (Setoid ((==)))
import Pandora.Pattern.Object.Chain (Chain ((<=>)))
import Pandora.Pattern.Object.Semigroup (Semigroup ((+)))
import Pandora.Pattern.Object.Ringoid (Ringoid ((*)))
import Pandora.Pattern.Object.Monoid (Monoid (zero))
import Pandora.Pattern.Object.Quasiring (Quasiring (one))
import Pandora.Paradigm.Primary.Object.Boolean (Boolean (True, False))
import Pandora.Paradigm.Primary.Object.Ordering (Ordering (Less, Equal, Greater))

data Natural = Zero | Natural Natural

instance Setoid Natural where
	Zero == Zero = True
	Natural n == Natural m = n == m
	_ == _ = False

instance Chain Natural where
	Zero <=> Zero = Equal
	Zero <=> Natural _ = Less
	Natural _ <=> Zero = Greater
	Natural n <=> Natural m = n <=> m

instance Semigroup Natural where
	Zero + m = m
	Natural n + m = Natural $ n + m

instance Ringoid Natural where
	Zero * _ = Zero
	Natural n * m = m + n * m

instance Monoid Natural where
	zero = Zero

instance Quasiring Natural where
	one = Natural Zero
