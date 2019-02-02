module Pandora.Paradigm.Controlflow.Pipeline (Pipeline) where

import Pandora.Paradigm.Basis.Continuation (Continuation (Continuation, continue))

newtype Source input t result = Input
	{ source :: Sink input t result -> t result }

newtype Sink output t result = Output
	{ sink :: output -> Source output t result -> t result }

newtype Tube input output result t a = Tube
	{ tube :: Source input t result -> Sink output t result -> t result }

type Pipeline input output t a result =
	Continuation result (Tube input output result t) a
