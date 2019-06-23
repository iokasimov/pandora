module Pandora.Paradigm.Basis.Tagged where

newtype Tagged tag a = Tagged a

untag :: Tagged tag a -> a
untag (Tagged x) = x

retag :: Tagged old a -> Tagged new a
retag (Tagged x) = Tagged x

tagself :: a -> Tagged a a
tagself = Tagged
