{-# LANGUAGE LambdaCase #-}
module Conversion where

import qualified Data.Map.Strict as M
import Data.Tuple
  (swap)
import Data.List.Extra
  (nubOrd)

import LambdaCalc
import DeBruijn

type NamedContext = M.Map Var Int
type NamelessContext = M.Map Int Var

switchContext :: NamedContext -> NamelessContext
switchContext = M.fromList . (fmap swap) . M.toList

-- FIXME
findName :: Var -> NamedContext  -> Int
findName var ctxt = case M.lookup var ctxt of
  Just n -> n
  Nothing -> error "Variable not defined"

findVar :: Int -> NamelessContext -> Var
findVar var ctxt = case M.lookup var ctxt of
  Just n -> n
  Nothing -> error "ahh"


insertName :: Var -> NamedContext -> NamedContext
insertName var ctxt =
  let ctxt' = M.map (+1) ctxt in
    M.insert var 0 ctxt'

insertVar :: Int -> Var  -> NamelessContext -> NamelessContext
insertVar int var ctxt =
  let ctxt' = M.mapKeys (+1) ctxt in
    M.insert int var ctxt'


toNameless :: (NamedContext, LTerm) -> DBLTerm
toNameless (ctxt, term) = case term of
  LVar var
    ->
      let nm = findName var ctxt in
        DVar nm
  LApp t1 t2
      ->
      DApp
        (toNameless (ctxt, t1))
        (toNameless (ctxt, t2))
  LAbs var body
    ->
      let ctxt' = insertName var ctxt in
        DAbs (toNameless (ctxt', body))


toNameless' :: LTerm -> DBLTerm
toNameless' term =
  let ctxt = makeContext term in
    toNameless (ctxt, term)


fromNameless :: (NamelessContext, DBLTerm) -> LTerm
fromNameless (ctxt, term) = case term of
  DVar nm
    ->
      let var = findVar nm ctxt in
        LVar var
  DApp t1 t2
    ->
      LApp
        (fromNameless (ctxt, t1))
        (fromNameless (ctxt, t2))
  DAbs bd
    ->
      let var = newVar varNames in
        let ctxt' = insertVar 0 var ctxt in
          LAbs var (fromNameless (ctxt', bd))
  where
    varNames :: [Var]
    varNames = fmap snd $ M.toList ctxt

makeContext :: LTerm -> NamedContext
makeContext lterm =
  let fVars = nubOrd $ freeVars lterm in
    M.fromList $ zip fVars [0..]
    
