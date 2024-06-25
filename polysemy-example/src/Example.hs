{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds, ScopedTypeVariables #-}
module Example (program) where
import Polysemy (makeSem, Member, Embed, Sem, interpret, embed, runM)

data Teletype m a where
  ReadTTY :: Teletype m String
  WriteTTY :: String -> Teletype m ()

makeSem ''Teletype

teletypeToIO :: Member (Embed IO) r => Sem (Teletype ': r) a -> Sem r a
teletypeToIO = interpret \case
  ReadTTY      -> embed getLine
  WriteTTY msg -> embed $ putStrLn msg

echo :: Member Teletype r => Sem r ()
echo = do
  i <- readTTY
  case i of
    "" -> pure ()
    _  -> writeTTY i >> echo

program :: IO ()
program = runM . teletypeToIO $ echo