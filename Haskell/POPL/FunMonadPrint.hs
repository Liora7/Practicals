module FunMonadPrint(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

infixl 1 $>

type M a = (String , a)

result :: a -> M a
result x = ("", x)

($>) :: M a -> (a -> M b) -> M b
($>) xm f = let (s,x)=xm in let (t,y)=(f x) in (s++t,y)

data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Function ([Value] -> M Value)

type Env = Environment Value

eval :: Expr -> Env -> M Value

eval (Number n) env = result (IntVal n)

eval (Variable x) env = result (find env x)

eval (Apply f es) env = 
  eval f env $> (\ fv ->
    evalargs es env $> (\ args ->
      apply fv args))

eval (Lambda xs e1) env =
  result (abstract xs e1 env)

eval (If e1 e2 e3) env =
  eval e1 env $> (\ b ->
    case b of
      BoolVal True -> eval e2 env
      BoolVal False -> eval e3 env
      _ -> error ("boolean required in conditional"))

eval (Let d e1) env =
  elab d env $> (\ env' -> eval e1 env')

eval (Sequence e1 e2) env =
  eval e1 env $> (\ v -> eval e2 env)

eval (While e1 e2) env = u
  where
    u = eval e1 env $> (\ v1 ->
      case v1 of
  	BoolVal True -> eval e2 env $> (\ v2 -> u)
	BoolVal False -> result Nil
	_ -> error "boolean required in while loop")

eval e env =
  error ("can't evaluate " ++ pretty e)

abstract :: [Ident] -> Expr -> Env -> Value
abstract xs e env =
  Function (\ args -> eval e (defargs env xs args))

apply :: Value -> [Value] -> M Value
apply (Function f) args = f args
apply _ args = error "applying a non-function"

elab :: Defn -> Env -> M Env
elab (Val x e) env = 
  eval e env $> (\ v -> result (define env x v))
elab (Rec x (Lambda xs e1)) env =
  result env' where env' = define env x
                               (abstract xs e1 env')
elab (Rec x _) env =
  error "RHS of letrec must be a lambda"
evalargs :: [Expr] -> Env -> M [Value]
evalargs [] env = result []
evalargs (e:es) env =
  eval e env $> (\ v -> evalargs es env
                        $> (\ vs -> result (v:vs)))

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
    pureprim ":" (\ [a, b] -> Cons a b),
    primitive "print" ( \ [v] -> (show v,Nil))]
  where constant x v = (x, v)
        primitive x f = (x, Function (primwrap x f))
        pureprim x f = primitive x (result . f)

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


obey :: Phrase -> Env -> (String, Env)
obey (Calculate exp) env =
  let (s,v) = eval exp env in
  (s++"\n"++print_value v, env)
obey (Define def) env =
  let x = def_lhs def in
  let (s,env') = elab def env in
  (s++"\n"++print_defn env' x, env')

main = dialog funParser obey init_env
