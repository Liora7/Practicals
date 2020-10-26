{-# LANGUAGE DataKinds, TypeOperators, PolyKinds, GADTs #-}

module TFun where

-- A Haskell type whose inhabitants are Fun types
data Type =
    TInt
  | Arrow [Type] Type

-- A type of Fun values, for a given Fun type
-- This uses the Haskell DataKinds extension.
data Value :: Type -> * where
  IntVal :: Integer -> Value TInt
  Function :: (Value t -> Value t') -> Value (Arrow '[t] t') 

-- A typing context is just a list of types.
type Ctx = [Type]

-- Variables in a context are numbers -- the position in the list.
data Var :: Ctx -> Type -> * where
  VarZero :: Var (a ': ts) a
  VarSucc :: Var ts a -> Var (b ': ts) a

-- An environment has a value for every variable
data Env :: Ctx -> * where
  EmptyEnv :: Env '[]
  ConsEnv :: Value t -> Env ctx -> Env (t ': ctx)

-- Find a variable in an environment 
find :: Env ctx -> Var ctx t -> Value t
find (ConsEnv v _) VarZero = v
find (ConsEnv _ env) (VarSucc x) = find env x

-- A type of typed Fun expressions
data Expr :: Ctx -> Type -> * where
    Number :: Integer -> Expr ctx TInt          
    Variable :: Var ctx t -> Expr ctx t            
    Apply :: Expr ctx (Arrow '[t] t') -> [Expr ctx t] -> Expr ctx t'   
    Lambda :: Expr (t ': ctx) t' -> Expr ctx (Arrow '[t] t') 


-- The eval function looks exactly as before, but now type soundness is built in.

eval :: Expr ctx t -> Env ctx -> Value t

eval (Number n) env = IntVal n

eval (Variable x) env = find env x

eval (Apply e1 [e2]) env =
  let (Function f) = eval e1 env in f (eval e2 env)

eval (Lambda e1) env =
     Function (\arg -> eval e1 (ConsEnv arg env))
