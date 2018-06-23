{-# LANGUAGE GADTs #-}
module LNumberal where

import LambdaCalc

data Nat where
  Zero :: Nat
  Succ :: Nat -> Nat

interp :: Nat -> Int
interp n = case n of
  Zero -> 0
  Succ n' -> (+1) $ interp n'

display :: Nat -> String
display = show . interp

churchNumeral :: Nat -> LTerm
churchNumeral n = case n of
  zero -> undefined
  
