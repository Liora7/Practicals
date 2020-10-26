module FunCPSmini(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

data Value =
    IntVal Integer		
  | Function (Value -> (Value -> Answer) -> Answer)    

type Env = Environment Value

eval :: Expr -> Env -> (Value -> Answer) -> Answer

eval (Number n) env k = k (IntVal n)

eval (Variable x) env k = k (find env x)

eval (Apply e1 [e2]) env k =
  eval e1 env (\(Function f) -> eval e2 env (\x -> f x k))

eval (Lambda [x] e1) env k =
     k (Function (\arg k -> eval e1 (defargs env [x] [arg]) k))

init_env :: Env
init_env = 
  make_env []

instance Show Value where
  show (IntVal n) = show n
  show (Function _) = "<function>"

type Answer = (String , Env)

obey :: Phrase -> Env -> Answer

obey (Calculate exp) env =
  eval exp env (\v -> (print_value v , env))

main = dialog funParser obey init_env
