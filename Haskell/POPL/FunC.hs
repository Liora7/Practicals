module FunC(main) where
import Parsing
import FunSyntax
import FunParser
import Environment
import Memory

infixl 1 $>

type M a = Mem -> (a, Mem)

result :: a -> M a
result x mem = (x, mem)

($>) :: M a -> (a -> M b) -> M b
(xm $> f) mem =
  let (x, mem1) = xm mem in f x mem1

get :: Location -> M Value
get a mem = (contents mem a, mem)

put :: Location -> Value -> M ()
put a v mem = ((), update mem a v)

new :: M Location
new mem = let (a, mem') = fresh mem in (a, mem')
data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Addr Location
  | Function ([Value] -> M Value)

type Env = Environment Value
type Mem = Memory Value

ltranslate :: Expr -> Expr
ltranslate (Contents e) = etranslate e 
ltranslate (Variable x) = Variable x

etranslate :: Expr -> Expr
etranslate (Address x) = ltranslate x
etranslate (Contents e) = Apply (Variable "!") [(etranslate e)]
etranslate (Number n) = Number n
etranslate (Variable x) = if maybe_find init_env x == Nothing
                          then Apply (Variable "!") [(Variable x)]
                          else Variable x
etranslate (Apply (Variable x) es) =
                          if maybe_find init_env x == Nothing
                          then Apply (Variable x) (map ref (map etranslate es))
                          else Apply (Variable x) (map etranslate es)
etranslate (Apply e es) = Apply e (map ref (map etranslate es))
etranslate (Lambda xs e) = Lambda xs (etranslate e)
etranslate (If e1 e2 e3) = If (etranslate e1) (etranslate e2) (etranslate e3)
etranslate (Let d e) = Let (dtranslate d) (etranslate e)
etranslate (Sequence e e') = Sequence (etranslate e) (etranslate e')
etranslate (While e e') = While (etranslate e) (etranslate e')
etranslate (Assign e e') = Assign (ltranslate e) (etranslate e')

dtranslate :: Defn -> Defn
dtranslate (Val x e) = Val x (ref (etranslate e))
dtranslate (Rec x e) = Rec x (etranslate e)

ref :: Expr -> Expr
ref e = Let (Val "refdummy" (Apply (Variable "new") [])) (Sequence (Assign (Variable "refdummy") e) (Variable "refdummy"))

eval :: Expr -> Env -> M Value
eval e env = eval' (etranslate e) env

eval' :: Expr -> Env -> M Value

eval' (Number n) env = result (IntVal n)

eval' (Variable x) env = result (find env x)

eval' (Apply f es) env =
  eval' f env $> (\ fv ->
    eval'args es env $> (\ args ->
      apply fv args))

eval' (Lambda xs e1) env =
  result (abstract xs e1 env)

eval' (If e1 e2 e3) env =
  eval' e1 env $> (\ b ->
    case b of
      BoolVal True -> eval' e2 env
      BoolVal False -> eval' e3 env
      _ -> error "boolean required in conditional")

eval' (Let d e1) env =
  elab' d env $> (\ env' -> eval' e1 env')

eval' (Sequence e1 e2) env =
  eval' e1 env $> (\ v -> eval' e2 env)

eval' (While e1 e2) env = u
  where
    u = eval' e1 env $> (\ v1 ->
      case v1 of
  	BoolVal True -> eval' e2 env $> (\ v2 -> u)
	BoolVal False -> result Nil
	_ -> error "boolean required in while loop")

eval' (Assign e1 e2) env =
  eval' e1 env $> (\ v1 ->
    case v1 of
      Addr a ->
        eval' e2 env $> (\ v2 -> put a v2 $> (\ () -> result v2))
      _ -> error "assigning to a non-variable")
eval' e env =
  error ("can't eval'uate " ++ pretty e)

abstract :: [Ident] -> Expr -> Env -> Value
abstract xs e env =
  Function (\ args -> eval' e (defargs env xs args))

apply :: Value -> [Value] -> M Value
apply (Function f) args = f args
apply _ args = error "applying a non-function"

elab :: Defn -> Env -> M Env
elab d env = elab' (dtranslate d) env 

elab' :: Defn -> Env -> M Env
elab' (Val x e) env = 
  eval' e env $> (\ v -> result (define env x v))
elab' (Rec x (Lambda xs e1)) env =
  result env' where env' = define env x (abstract xs e1 env')
elab' (Rec x _) env =
  error "RHS of letrec must be a lambda"
eval'args :: [Expr] -> Env -> M [Value]
eval'args [] env = result []
eval'args (e:es) env =
  eval' e env $> (\ v -> eval'args es env $> (\ vs -> result (v:vs)))

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
    primitive "!" (\ [Addr a] -> get a),
    primitive "new" (\ [] -> new $> (\ a -> result (Addr a)))]
  where constant x v = (x, v)
        primitive x f = (x, Function (primwrap x f))
        pureprim x f = primitive x (result . f)

instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = (h1 == h2) && (t1 == t2)
  Function _ == Function _ = error "can't compare functions"
  Addr a == Addr b = a == b
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
  show (Addr a) = "<address " ++ show a ++ ">"

type GloState = (Env, Mem)

obey :: Phrase -> GloState -> (String, GloState)
obey (Calculate exp) (env, mem) =
  let (v, mem') = eval exp env mem in
  (print_value v, (env, mem'))
obey (Define def) (env, mem) =
  let x = def_lhs def in
  let (env', mem') = elab def env mem in
  (print_defn env' x, (env', mem'))

main = dialog funParser obey (init_env, init_mem)
