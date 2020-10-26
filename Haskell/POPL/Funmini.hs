module FunCPSmini(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

data Value =
    IntVal Integer		
  | Function (Value -> Value)

type Env = Environment Value

eval :: Expr -> Env -> Value

eval (Number n) env = IntVal n

eval (Variable x) env = find env x

eval (Apply e1 [e2]) env =
  let (Function f) = eval e1 env in f (eval e2 env)

eval (Lambda [x] e1) env =
     Function (\arg -> eval e1 (defargs env [x] [arg]))

init_env :: Env
init_env = 
  make_env []

instance Show Value where
  show (IntVal n) = show n
  show (Function _) = "<function>"

obey :: Phrase -> Env -> (String, Env)

obey (Calculate exp) env =
     print_value (eval exp env) , env

main = dialog funParser obey init_env
