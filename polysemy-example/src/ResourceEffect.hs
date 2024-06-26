{-# LANGUAGE TemplateHaskell, LambdaCase, BlockArguments, GADTs
           , FlexibleContexts, TypeOperators, DataKinds, PolyKinds
           , TypeApplications #-}

module ResourceEffect (runProgram) where
import Polysemy (Members, Sem, runFinal, embedToFinal)
import Polysemy.Resource (Resource, bracket, resourceToIOFinal)
import Polysemy.Error (Error, catch, throw, errorToIOFinal)
import TeletypeEffect (Teletype, writeTTY, readTTY, teletypeToIO)

data CustomeException = ThisException | ThatException deriving Show

program :: Members '[Resource, Teletype, Error CustomeException] r => Sem r ()
program = catch @CustomeException work \e -> writeTTY $ "Caught " ++ show e
  where
    work = bracket readTTY (const $ writeTTY "existing bracket") \input -> do
      writeTTY "entering bracket"
      case input of
        "explode" -> throw ThisException
        "weird stuff" -> writeTTY input *> throw ThatException
        _ -> writeTTY input *> writeTTY "no exceptions"

runProgram :: IO (Either CustomeException ())
runProgram
  = runFinal
  . embedToFinal @IO
  . resourceToIOFinal
  . errorToIOFinal @CustomeException
  . teletypeToIO
  $ program