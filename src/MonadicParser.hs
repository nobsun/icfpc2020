module MonadicParser (
  Parser,
  runParser,
  token,
  satisfy,
  eof,
  ) where

import Control.Applicative (empty)
import Control.Monad (MonadPlus, guard)
import Control.Monad.Trans.State (StateT, runStateT, get, put)


type Parser t m = StateT [t] m

runParser :: Parser t m a -> [t] -> m (a, [t])
runParser = runStateT

token :: MonadPlus m => Parser t m t
token = do
  tts <- get
  case tts of
    []    ->  empty
    t:ts  ->  put ts *> pure t

satisfy :: MonadPlus m => (t -> Bool) -> Parser t m t
satisfy p = do
  t <- token
  guard $ p t
  return t

eof :: MonadPlus m => Parser t m ()
eof = do
  tts <- get
  case tts of
    []   ->  pure ()
    _:_  ->  empty
