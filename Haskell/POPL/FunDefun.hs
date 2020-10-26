module FunDefun(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Closure [Ident] Expr Env		-- Function closures
  | Primitive Prim			-- Primitives

data Prim = 
    Plus | Minus | Times | Div | Mod | Uminus | Less | Greater
  | Leq | Geq | Equal | Neq | Integer | Head | Tail | Consop
  deriving Show

type Env = Environment Value

eval :: Expr -> Env -> Value

eval (Number n) env = IntVal n

eval (Variable x) env = find env x

eval (If e1 e2 e3) env =
  case eval e1 env of
    BoolVal True -> eval e2 env
    BoolVal False -> eval e3 env
    _ -> error "boolean required in conditional"

eval (Apply f es) env =
  apply (eval f env) (map ev es)
    where ev e1 = eval e1 env

eval (Lambda xs e1) env = abstract xs e1 env

eval (Let d e1) env = eval e1 (elab d env)

eval e env =
  error ("can't evaluate " ++ pretty e)

apply :: Value -> [Value] -> Value
apply (Closure xs e env) args =
               eval e (defargs env xs args)
apply (Primitive p) args = primapply p args 
apply _ args = error "applying a non-function"

abstract :: [Ident] -> Expr -> Env -> Value
abstract xs e env = Closure xs e env

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (eval e env)

elab (Rec x (Lambda xs e1)) env =
  env' where env' = define env x (abstract xs e1 env')
elab (Rec x _) env =
  error "RHS of letrec must be a lambda"

init_env :: Env
init_env = 
  make_env [constant "nil" Nil, 
            constant "true" (BoolVal True), 
            constant "false" (BoolVal False),
    primitive "+" Plus, primitive "-" Minus, primitive "*" Times,
    primitive "div" Div, primitive "mod" Mod, primitive "~" Uminus,
    primitive "<" Less, primitive ">" Greater, primitive ">=" Geq, 
    primitive "<=" Leq, primitive "=" Equal, primitive "<>" Neq,  
    primitive "integer" Integer, primitive "head" Head, 
    primitive "tail" Tail, primitive ":" Consop]
  where
    constant x v = (x, v)
    primitive x p = (x, Primitive p)

primapply :: Prim -> [Value] -> Value
primapply Plus [IntVal a, IntVal b] = IntVal (a + b)
primapply Minus [IntVal a, IntVal b] = IntVal (a - b)
primapply Times [IntVal a, IntVal b] = IntVal (a * b)
primapply Div [IntVal a, IntVal b] = 
  if b == 0 then error "dividing by zero" 
  else IntVal (a `div` b)
primapply Mod [IntVal a, IntVal b] =
  if b == 0 then error "dividing by zero" 
  else IntVal (a `mod` b)
primapply Uminus [IntVal a] = IntVal (- a)
primapply Less [IntVal a, IntVal b] = BoolVal (a < b)
primapply Leq [IntVal a, IntVal b] = BoolVal (a <= b)
primapply Greater [IntVal a, IntVal b] = BoolVal (a > b)
primapply Geq [IntVal a, IntVal b] = BoolVal (a >= b)
primapply Equal [a, b] = BoolVal (a == b)
primapply Neq [a, b] = BoolVal (a /= b)
primapply Integer [a] =
  case a of 
    IntVal _ -> BoolVal True
    _ -> BoolVal False
primapply Head [Cons h t] = h
primapply Tail [Cons h t] = t
primapply Consop [a, b] = Cons a b
primapply x args = 
  error ("bad arguments to primitive " ++ show x ++ ": " 
						++ showlist args)


instance Eq Value where
  IntVal a == IntVal b = a == b
  BoolVal a == BoolVal b = a == b
  Nil == Nil = True
  Cons h1 t1 == Cons h2 t2 = (h1 == h2) && (t1 == t2)
  f == g | is_function f && is_function g = 
    error "can't compare functions"
  _ == _ = False

is_function :: Value -> Bool
is_function (Closure _ _ _) = True
is_function (Primitive _) = True
is_function _ = False

instance Show Value where
  show (IntVal n) = show n
  show (BoolVal b) = if b then "true" else "false"
  show Nil = "[]"
  show (Cons h t) = "[" ++ show h ++ shtail t ++ "]"
    where 
      shtail Nil = ""
      shtail (Cons h t) = ", " ++ show h ++ shtail t
      shtail x = " . " ++ show x
  show (Closure _ _ _) = "<function>"
  show (Primitive x) = "<primitive " ++ show x ++ ">"

obey :: Phrase -> Env -> (String, Env)

obey (Calculate exp) env =
  (print_value (eval exp env), env)

obey (Define def) env =
  let x = def_lhs def in
  let env' = elab def env in
  (print_defn env' x, env')

main = dialog funParser obey init_env
