module Pandora.Core.Impliable where

class Impliable result where
	type Arguments result = args | args -> result
	imply :: Arguments result
