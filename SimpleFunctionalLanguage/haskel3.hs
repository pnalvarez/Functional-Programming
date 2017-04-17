-----------------------------                                             -- 001
-- Simple Functional Language                                             -- 002
-----------------------------                                             -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
-- values are integers and functions                                      -- 009
data Value = ValInt Integer                                               -- 010
           | ValFunc (Value -> Value)                                     -- 011
           | ValError String                                              -- 012
                                                                          -- 013
                                                                          -- 014
-- an Environment maps variables to Values                                -- 015
type Env = Var -> Value                                                   -- 016
                                                                          -- 017
                                                                          -- 018
-- auxiliary function to map Values to Booleans                           -- 019
isTrue :: Value -> Bool                                                   -- 020
isTrue (ValInt i) = (i /= 0)                                              -- 021
                                                                          -- 022
                                                                          -- 023
-- An empty Environment                                                   -- 024
emptyEnv :: Env                                                           -- 025
emptyEnv v = ValError ("undefined variable " ++ v)                        -- 026
                                                                          -- 027
                                                                          -- 028
-- bind a new value in an environment                                     -- 029
bind :: Var -> Value -> Env -> Env                                        -- 030
bind var val env = \v -> if var == v then val else env v                  -- 031
                                                                          -- 032
                                                                          -- 033
-- executes a binary operation on values                                  -- 034
binOp :: (Integer -> Integer -> Integer) -> Value -> Value -> Value       -- 035
binOp op (ValInt i1) (ValInt i2) = ValInt (op i1 i2)                      -- 036
binOp _ _ _ = ValError "binary operand is not a number"                   -- 037
                                                                          -- 038
                                                                          -- 039
fix :: (a -> a) -> a                                                      -- 040
fix f = x where x = f x                                                   -- 041
                                                                          -- 042
--------------------------------------------------------------------      -- 043
-- Abstract Syntax Tree for Expressions                                   -- 044
data Exp = ExpK Integer          -- constants                             -- 045
         | ExpVar Var            -- variables                             -- 046
         | ExpAdd Exp Exp        -- e1 + e2                               -- 047
         | ExpSub Exp Exp        -- e1 - e2                               -- 048
         | ExpMul Exp Exp        -- e1 * e2                               -- 049
         | ExpDiv Exp Exp        -- e1 / e2                               -- 050
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 -- 051
         | ExpApp Exp Exp        -- e1 e2                                 -- 052
         | ExpLambda Var Exp     -- \x -> e                               -- 053
         | ExpLet Var Exp Exp    -- letrec x=e1 in e2                     -- 054
                                                                          -- 055
-- Evaluates an expression in a given environment                         -- 056
evalExp :: Exp -> Env -> Value                                            -- 057
                                                                          -- 058
evalExp (ExpK i) env = ValInt i                                           -- 059
evalExp (ExpVar v) env = env v                                            -- 060
evalExp (ExpAdd e1 e2) env = binOp (+) (evalExp e1 env) (evalExp e2 env)  -- 061
evalExp (ExpSub e1 e2) env = binOp (-) (evalExp e1 env) (evalExp e2 env)  -- 062
evalExp (ExpMul e1 e2) env = binOp (*) (evalExp e1 env) (evalExp e2 env)  -- 063
evalExp (ExpDiv e1 e2) env = binOp div (evalExp e1 env) (evalExp e2 env)  -- 064
evalExp (ExpIf e1 e2 e3) env =                                            -- 065
    if isTrue(evalExp e1 env) then evalExp e2 env else evalExp e3 env     -- 066
evalExp (ExpApp e1 e2) env =                                              -- 067
  case (evalExp e1 env) of                                                -- 068
    ValFunc f -> f (evalExp e2 env)                                       -- 069
    _         -> ValError "calling a non-function value"                  -- 070
evalExp (ExpLambda v e) env = ValFunc (\x -> evalExp e (bind v x env))    -- 071
                                                                          -- 072
evalExp (ExpLet v e1 e2) env = evalExp e2 env'                            -- 073
  where env' = bind v (evalExp e1 env') env                               -- 074
                                                                          -- 075
                                                                          -- 076
---------------------------------------------------------------------------- 077
-------------------------------------------------------------------       -- 078
-- some examples                                                          -- 079
                                                                          -- 080
-- letrec f = \x -> if x then x * f(x - 1) else 1                         -- 081
-- in f 10                                                                -- 082
fat =                                                                     -- 083
  ExpLet "f"                                                              -- 084
    (ExpLambda "x"                                                        -- 085
       (ExpIf (ExpVar "x")                                                -- 086
              (ExpMul (ExpVar "x")                                        -- 087
                      (ExpApp (ExpVar "f") (ExpSub (ExpVar "x") (ExpK 1))))-- 088
              (ExpK 1)))                                                  -- 089
    (ExpApp (ExpVar "f") (ExpK 10))                                       -- 090
                                                                          -- 091
                                                                          -- 092
-- y = \f -> (\x -> f (x x)) (\x -> f (x x))                              -- 093
y = ExpLambda "f" (ExpApp y' y')                                          -- 094
  where y' = ExpLambda "x"                                                -- 095
               (ExpApp (ExpVar "f") (ExpApp (ExpVar "x") (ExpVar "x")))   -- 096
                                                                          -- 097
-- fatG = \f -> \x -> if x then x * f(x - 1) else 1                       -- 098
fatG =                                                                    -- 099
  ExpLambda "f"                                                           -- 100
    (ExpLambda "x"                                                        -- 101
       (ExpIf (ExpVar "x")                                                -- 102
              (ExpMul (ExpVar "x")                                        -- 103
                      (ExpApp (ExpVar "f") (ExpSub (ExpVar "x") (ExpK 1))))-- 104
              (ExpK 1)))                                                  -- 105
                                                                          -- 106
-- fat' = (y fatG) 20                                                     -- 107
fat' = ExpApp (ExpApp y fatG) (ExpK 20)                                   -- 108
                                                                          -- 109
-- code to show the final value of an expression                          -- 110
main :: IO ()                                                             -- 111
main = ((case (evalExp fat' emptyEnv) of                                  -- 112
                         ValInt i -> print i                              -- 113
                         ValFunc _ -> print "function"                    -- 114
                         ValError err -> print ("error: " ++ err)))       -- 115
                                                                          -- 116