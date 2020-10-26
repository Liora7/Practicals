module FunCK(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

data Value =
    IntVal Integer		
  | Closure Ident Expr Env

applyValue :: Value -> Value -> Cont -> Answer
applyValue (Closure x e env) arg k = eval e (defargs env [x] [arg]) k

data Cont = ContArg Expr Env Cont
          | ContApp Value Cont
          | Show Env
applyCont :: Cont -> Value -> Answer
applyCont (ContArg e2 env k) f = eval e2 env (ContApp f k)
applyCont (ContApp f k) x = applyValue f x k
applyCont (Show env) val = (show val , env)

type Env = Environment Value

eval :: Expr -> Env -> Cont -> Answer

eval (Number n) env k = applyCont k (IntVal n)

eval (Variable x) env k = applyCont k (find env x)

eval (Apply e1 [e2]) env k =
  eval e1 env (ContArg e2 env k)

eval (Lambda [x] e1) env k =  applyCont k (Closure x e1 env)

init_env :: Env
init_env = 
  make_env []

instance Show Value where
  show (IntVal n) = show n
  show (Closure _ _ _) = "<function>"

type Answer = (String , Env)

obey :: Phrase -> Env -> Answer

obey (Calculate exp) env =
  eval exp env (Show env)

main = dialog funParser obey init_env
