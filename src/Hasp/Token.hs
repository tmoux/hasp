module Hasp.Token where

{-
Each token has (possibly) a payload, the data that comes with the token.
Only the tokens are being matched on, not the payload.

Currently, the type of the char parser is
char :: t -> Hoas t t
(Given a token, create a parser that returns a t)

Maybe the way to go is a GADT, where each data constructor can have a type (or unit) representing the type of its payload.

Streams should return a Maybe (t a, a, s)
-}



class Token t where
  payload :: t a -> a
