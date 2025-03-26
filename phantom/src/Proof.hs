{-# LANGUAGE PolyKinds #-}
module Proof where

data Proof (pf :: k) = QED

axiom :: Proof p
axiom = QED