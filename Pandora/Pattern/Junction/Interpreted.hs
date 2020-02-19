module Pandora.Pattern.Junction.Interpreted (Interpreted (..)) where

class Interpreted t where
	{-# MINIMAL unwrap #-}
	type Primary t a :: *
	unwrap :: t a -> Primary t a
