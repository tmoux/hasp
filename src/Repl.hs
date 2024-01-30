module Repl where

import Control.Monad.State
import Text.Parsec hiding (State)

type MyParser a = ParsecT String () (State Int) a

charParser :: MyParser Char
charParser = do
  currentState <- lift get
  charValue <- anyChar
  when (currentState > 5) $ parserFail "too long"
  lift (modify (+ 1))
  return charValue

parseAndCount :: String -> (Either ParseError [Char], Int)
parseAndCount input = runState (runParserT (many charParser) () "" input) 0
