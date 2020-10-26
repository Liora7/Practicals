module Fun(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Function ([Value] -> (Value -> Answer) -> Answer)         -- Functions

-- An environment is a map from identifiers to values
type Env = Environment Value

eval :: Expr -> Env -> (Value -> Answer) -> Answer

eval (Number n) env k = k (IntVal n)

eval (Variable x) env k = k (find env x)

eval (If e1 e2 e3) env k =
  eval e1 env (\x -> case x of 
    BoolVal True -> eval e2 env k
    BoolVal False -> eval e3 env k
    _ -> error "boolean required in conditional")

eval (Apply e1 [e2]) env k =
  eval e1 env (\f -> eval e2 env (\x -> k (f x)))

eval (Lambda xs e1) env k = abstract xs e1 env k

eval (Let d e1) env k = elab d env (\x -> eval e1 x k)

eval e env =
  error ("can't evaluate " ++ pretty e)


abstract :: [Ident] -> Expr -> Env -> (Value -> Answer) -> Answer
abstract xs e env k = 
  k (Function (\args k -> eval e (defargs env xs args) k))

elab :: Defn -> Env -> (Env -> Answer) -> Answer
elab (Val x e) env k = eval e env (\v -> k (define env x v))

-- elab (Rec x (Lambda xs e1)) env k =
--  k env' where abstract xs e1 env' (\f -> define env x () f)
elab (Rec x _) env k =
  error "RHS of letrec must be a lambda"

init_env :: Env
init_env = 
  make_env [constant "nil" Nil, 
            constant "true" (BoolVal True), 
            constant "false" (BoolVal False),
    pureprim "+" (\ [IntVal a, IntVal b] -> IntVal (a + b)),
    pureprim "-" (\ [IntVal a, IntVal b] -> IntVal (a - b)),
    pureprim "*" (\ [IntVal a, IntVal b] -> IntVal (a * b)),
    pureprim "div" (\ [IntVal a, IntVal b] -> 
      if b == 0 then error "Dividing by zero" else IntVal (a `div` b)),
    pureprim "mod" (\ [IntVal a, IntVal b] ->
      if b == 0 then error "Dividing by zero" else IntVal (a `mod` b)),
    pureprim "~" (\ [IntVal a] -> IntVal (- a)),
    pureprim "<" (\ [IntVal a, IntVal b] -> BoolVal (a < b)),
    pureprim "<=" (\ [IntVal a, IntVal b] -> BoolVal (a <= b)),
    pureprim ">" (\ [IntVal a, IntVal b] -> BoolVal (a > b)),
    pureprim ">=" (\ [IntVal a, IntVal b] -> BoolVal (a >= b)),
    pureprim "=" (\ [a, b] -> BoolVal (a == b)),
    pureprim "<>" (\ [a, b] -> BoolVal (a /= b)),
    pureprim "integer" (\ [a] ->
      case a of IntVal _ -> BoolVal True; _ -> BoolVal False),
    pureprim "head" (\ [Cons h t] -> h),
    pureprim "tail" (\ [Cons h t] -> t),
    pureprim ":" (\ [a, b] -> Cons a b)]
  where constant x v = (x, v)
        pureprim x f = (x, Function (primwrap x (\k -> k (f x))))

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = (h1 == h2) && (t1 == t2)
  Function _ == Function _ = error "can't compare functions"
  _ == _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h t) = ", " ++ show h ++ shtail t
      shtail x = " . " ++ show x
  show (Function _) = "<function>"

type Answer = (String , Env)

obey :: Phrase -> Env -> Answer

obey (Calculate exp) env =
  eval exp env (\v -> (print_value v , env))

obey (Define def) env =
  let x = def_lhs def in
  elab def env (\env' -> (print_defn env' x, env'))

main = dialog funParser obey init_env
