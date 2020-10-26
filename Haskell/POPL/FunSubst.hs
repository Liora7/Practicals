module FunSubst(main) where
import Parsing
import FunSyntax
import FunParser
import Environment

data Value =
    IntVal Integer			-- Integers
  | BoolVal Bool			-- Booleans
  | Nil 				-- Empty list
  | Cons Value Value			-- Non-empty lists
  | Closure [Ident] Expr		-- Function closures
  | Primitive Prim			-- Primitives

data Prim = 
    Plus | Minus | Times | Div | Mod | Uminus | Less | Greater
  | Leq | Geq | Equal | Neq | Integer | Head | Tail | Consop
  deriving Show

valtoexp :: Value -> Expr
valtoexp (IntVal n) = Number n
valtoexp (BoolVal True) = Variable "true"
valtoexp (BoolVal False) = Variable "false"
valtoexp (Nil) = Variable "nil"
valtoexp (Cons v w) = Apply (Variable "cons") [(valtoexp v), (valtoexp w)]
valtoexp (Closure xs e) = Lambda xs e

fix :: Expr
fix = Lambda ["f"] (Apply (Lambda ["x"] (Apply (Variable "f") [Apply (Variable "x") [Variable "x"]])) [Lambda ["x"] (Apply (Variable "f") [Apply (Variable "x") [Variable "x"]])])

type Env = Environment Value

eval :: Expr -> Value

eval (Number n) = IntVal n

eval (If e1 e2 e3) =
  case eval e1 of
    BoolVal True -> eval e2
    BoolVal False -> eval e3
    _ -> error "boolean required in conditional"

eval (Apply f es)=
  apply (eval f) (map eval es)

eval (Lambda xs e1) = abstract xs e1

eval (Let d e1) = eval (defsubst d e1)

eval e =
  error ("can't evaluate " ++ pretty e)

apply :: Value -> [Value] -> Value
apply (Closure xs e) args = eval (substexprs xs args e)
apply (Primitive p) args = primapply p args 
apply _ args = error "applying a non-function"

abstract :: [Ident] -> Expr -> Value
abstract xs e = Closure xs e


substexpr :: Ident -> Value -> Expr -> Expr
substexpr x v (Number n)= Number n
substexpr x v (Variable y) = if x==y then valtoexp v else Variable y
substexpr x v (If e1 e2 e3) = If (substexpr x v e1) (substexpr x v e2) (substexpr x v e3)
substexpr x v (Apply f es) = Apply (substexpr x v f) (map (substexpr x v) es)
substexpr x v (Lambda xs e1) = if elem x xs then (Lambda xs e1) else (Lambda xs (substexpr x v e1))
substexpr x v (Let (Val y e) e1) = if x==y then Let (Val y e) e1 else Let (Val y (substexpr x v e)) (substexpr x v e1)
substexpr x v (Let (Rec y e) e1) = if x==y then Let (Rec y e) e1 else Let (Rec y (substexpr x v e)) (substexpr x v e1)

substexprs :: [Ident] -> [Value] -> Expr -> Expr
substexprs nil _ e = e
substexprs (x : xs) (v : vs) e = substexpr x v (substexprs xs vs e)

defsubst :: Defn -> Expr -> Expr
defsubst (Val x e) e' = substexpr x (eval e) e'
defsubst (Rec x (Lambda xs e)) e' = substexpr x (Closure xs (Apply fix [Lambda [x] (Lambda xs e)])) e'

elab :: Defn -> Env -> Env
elab (Val x e) env = define env x (eval e)

elab (Rec x (Lambda xs e1)) env =
  env' where env' = define env x (abstract xs e1 )
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
is_function (Closure _ _) = True
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
  show (Closure _ _) = "<function>"
  show (Primitive x) = "<primitive " ++ show x ++ ">"

obey :: Phrase -> Env -> (String, Env)

obey (Calculate exp) env =
  (print_value subst(eval exp), env)

obey (Define def) env =
  let x = def_lhs def in
  let env' = elab def env in
  (print_defn env' x, env')

main = dialog funParser obey init_env
