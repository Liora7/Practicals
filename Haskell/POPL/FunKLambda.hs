module FunKLambda(main) where
import Parsing
import FunSyntax
import FunParser
import Environment


type M a = (a -> Answer) -> Answer 


data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Function ([Value] -> M Value)

type Env = Environment Value

eval :: Expr -> Env -> M Value

eval (Number n) env k = k (IntVal n)

eval (Variable x) env k = k (find env x)

eval (Apply f es) env k = 
  eval f env (\ fv ->
    evalargs es env (\ args ->
      apply fv args k))

eval (Lambda xs e1) env k = 
  k (abstract xs e1 env)

eval (If e1 e2 e3) env k = 
  eval e1 env (\ b ->
    case b of
      BoolVal True -> eval e2 env k
      BoolVal False -> eval e3 env k
      _ -> ("ERROR: boolean required in conditional",env))

eval e env k =
  ("can't evaluate " ++ pretty e,env)

evalargs :: [Expr] -> Env -> M [Value]
evalargs [] env k = k []
evalargs (e:es) env k =
  eval e env (\ v -> evalargs es env
                        (\ vs -> k (v:vs)))

abstract :: [Ident] -> Expr -> Env -> Value
abstract xs e env =
  Function (\ args -> eval e (defargs env xs args))

apply :: Value -> [Value] -> M Value
apply (Function f) args k = f args k
apply _ args k = error "applying a non-function"

elab :: Defn -> Env -> M Env
elab (Val x e) env k = 
  eval e env (\ v -> k (define env x v))
elab (Rec x (Lambda xs e1)) env k = 
  k env' where env' = define env x
                               (abstract xs e1 env')
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
        primitive x f = (x, Function (primwrap x f))
        pureprim x f = primitive x ((\v k -> k v) . f)

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = (h1 == h2) && (t1 == t2)
  Function _ == Function _ =
                      error "can't compare functions"
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

type Answer = (String, Env)

obey :: Phrase -> Env -> Answer
obey (Calculate exp) env =
  eval exp env (\v -> 
  (print_value v, env))
obey (Define def) env =
  let x = def_lhs def in
  elab def env (\env' -> 
  (print_defn env' x, env'))

main = dialog funParser obey init_env
