module Main where

import Conversion
import LambdaCalc
import DeBruijn

main :: IO ()
main = do
  let zeroD = toNameless' testT
  print $ eval $  zeroD
  print $ cycleSucc (DAbs (DVar 0))
  let oneD = succD zeroD
  let
    s = DVar 1
    z = DVar 0
    ev = \t -> eval (DApp (DApp t s) z)
  print $ oneD
  print $ smallStep $ (DApp oneD s)
  let fourD =  eval $ succD $ succD $ succD  oneD
  let ctxt = makeContext testT
  let ctxt' = switchContext ctxt
  print $ fromNameless (ctxt', fourD)
