{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
module LambdaCalc where

data Sym = X | Y | Z | W | S
  deriving (Eq, Ord)

instance Show Sym where
  show X = "x"
  show Y = "y"
  show Z = "z"
  show W = "w"
  show S = "s"

newtype Var = Var {unVar :: (Sym, Int)}
  deriving (Eq, Ord)


s0 =  Var (S,0)

z0 = Var (Z,0)

tick :: Var -> Var
tick = Var . (\(v,n) -> (v, n + 1)) . unVar

newVar :: [Var] -> Var
newVar = \case
  []   -> Var (X,0)
  vs   -> tick maxVar
    where maxVar = maximum vs


instance Show Var where
  show (Var (sym, ind))  = case ind of
    0 -> show sym
    _ -> show sym ++ show ind

data LTerm where
  LVar :: Var   -> LTerm
  LAbs :: Var   -> LTerm -> LTerm
  LApp :: LTerm -> LTerm -> LTerm
  deriving (Eq)

printT :: LTerm -> String
printT = \case
  LVar var
    -> show var
  LAbs var body
    -> "\\" ++ show var ++ " . " ++ printT body
  LApp t1 t2
    ->
      "" ++ (printT t1) ++ " " ++
      "" ++ (printT t2) ++ ""

instance Show LTerm where
  show = printT

testT = LAbs s0 $ LAbs z0 (LVar z0)
succ :: LTerm -> LTerm
succ t = undefined --LAbs s0 $ LAbs z0 $ LApp (LVar s0) (LApp (LApp t LVar s0) LVar z0)

freeVars :: LTerm -> [Var]
freeVars = \case
  LVar var           -> [var]
  LApp lterm1 lterm2 -> freeVars lterm1 ++ freeVars lterm2
  LAbs var body      -> filter (/= var) $ freeVars body

boundVars :: LTerm -> [Var]
boundVars = \case
  LVar var           -> []
  LApp lterm1 lterm2 -> boundVars lterm1 ++ boundVars lterm2
  LAbs var body      -> var : boundVars body


         -- | -  to be read as: [var -> sub] term
naiveSubst :: Var -> LTerm -> LTerm -> LTerm
naiveSubst var sub  = \case
  LVar v | v == var -> sub
  LVar w            -> LVar w
  LApp l1 l2        -> LApp (naiveSubst var sub l1) (naiveSubst var sub l2)
  LAbs var body     -> LAbs var (naiveSubst var sub body)

subst :: Var -> LTerm -> LTerm -> LTerm
subst var sub term = let sub' = cleanVars term sub in
  naiveSubst var sub' term

             -- | - term we are substituting into
             -- |
             -- |      --| - term we are substituting
             -- |      --|
cleanVars :: LTerm -> LTerm -> LTerm
cleanVars term = \case
  LVar v               -> case elem v bVars of
                            True  -> LVar (newVar bVars)
                            False -> LVar v
  LApp lterm1 lterm2   -> LApp (cleanVars term lterm1) (cleanVars term lterm2)
  t  @(LAbs bVar body) -> let nVar = newVar fVars in
                            case elem bVar fVars of
                              True  -> cleanVars term (renameVar bVar nVar t)
                              False -> LAbs bVar (cleanVars term body)

  where bVars = boundVars term
        fVars = freeVars term

renameVar :: Var -> Var -> LTerm -> LTerm
renameVar oVar nVar = \case
  LVar var | var == oVar        -> LVar nVar
  LVar var                      -> LVar var
  LApp l1 l2                    -> LApp (renameVar oVar nVar l1) (renameVar oVar nVar l2)
  LAbs bVar body | bVar == nVar -> LAbs nVar (renameVar oVar nVar body)
  LAbs  bVar body               -> LAbs bVar (renameVar oVar nVar body)
