---------------------------------------------                             -- 001
-- Funcional language with a nice abstraction                             -- 002
---------------------------------------------                             -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
-- values are integers and functions                                      -- 009
data Value = ValInt Int                                                   -- 010
           | ValFunc (Value -> Cmpt Value)                                -- 011
                                                                          -- 012
-- final results are strings (to be printed)                              -- 013
type Result = String                                                      -- 014
                                                                          -- 015
                                                                          -- 016
-- continuations                                                          -- 017
type K a = a -> Result                                                    -- 018
                                                                          -- 019
                                                                          -- 020
-- a computation gets a continuation and gives a result                   -- 021
type Cmpt a = K a -> Result                                               -- 022
                                                                          -- 023
-- transforms a value into a computation                                  -- 024
op0 :: a -> Cmpt a                                                        -- 025
op0 x = \k -> k x                                                         -- 026
                                                                          -- 027
-- executes an unary operation on computations                            -- 028
op1 :: Cmpt a -> (a -> Cmpt b) -> Cmpt b                                  -- 029
op1 ca op = \k -> ca (\a -> op a k)                                       -- 030
                                                                          -- 031
-- computation error                                                      -- 032
cerror :: String -> Cmpt a                                                -- 033
cerror s k = s                                                            -- 034
                                                                          -- 035
                                                                          -- 036
-- an Environment maps variables to Values through continuations          -- 037
type Env = Var -> Cmpt Value                                              -- 038
                                                                          -- 039
                                                                          -- 040
-- An empty Environment                                                   -- 041
emptyEnv :: Env                                                           -- 042
emptyEnv v = cerror ("undefined variable " ++ v)                          -- 043
                                                                          -- 044
                                                                          -- 045
-- bind a new value in an environment                                     -- 046
bind :: Var -> Value -> Env -> Env                                        -- 047
bind var val env = \v -> if (var == v) then (op0 val) else (env v)        -- 048
                                                                          -- 049
                                                                          -- 050
-- executes a binary operation on computations                            -- 051
op2 :: (a -> b -> Cmpt c) -> Cmpt a -> Cmpt b -> Cmpt c                   -- 052
op2 op ca cb = op1 ca (\a -> op1 cb (op a))                               -- 053
                                                                          -- 054
-- executes a binary integer operation on computations                    -- 055
arith :: (Int -> Int -> Int) -> Cmpt Value -> Cmpt Value -> Cmpt Value    -- 056
arith op = op2 op_aux                                                     -- 057
  where op_aux (ValInt i1) (ValInt i2) = op0 (ValInt (op i1 i2))          -- 058
        op_aux _ _ = cerror "binary operation over non-int value"         -- 059
                                                                          -- 060
                                                                          -- 061
--------------------------------------------------------------------      -- 062
-- Abstract Syntax Tree for Expressions                                   -- 063
data Exp = ExpK Int              -- constants                             -- 064
         | ExpVar Var            -- variables                             -- 065
         | ExpAdd Exp Exp        -- e1 + e2                               -- 066
         | ExpSub Exp Exp        -- e1 - e2                               -- 067
         | ExpMul Exp Exp        -- e1 * e2                               -- 068
         | ExpDiv Exp Exp        -- e1 / e2                               -- 069
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 -- 070
         | ExpApp Exp Exp        -- e1 e2                                 -- 071
         | ExpLambda Var Exp     -- \x -> e                               -- 072
         | ExpLet Var Var Exp Exp        -- letrec x=(\x'->e') in e       -- 073
                                                                          -- 074
-- creates a closure in given environment                                 -- 075
closure :: Var -> (Env -> Cmpt Value) -> Env -> Value                     -- 076
closure v e env = ValFunc (\x -> e (bind v x env))                        -- 077
                                                                          -- 078
                                                                          -- 079
-- Evaluates an expression in a given environment                         -- 080
evalExp :: Exp -> Env -> Cmpt Value                                       -- 081
                                                                          -- 082
evalExp (ExpK i) env = op0 (ValInt i)                                     -- 083
evalExp (ExpVar v) env = env v                                            -- 084
evalExp (ExpAdd e1 e2) env = arith (+) (evalExp e1 env) (evalExp e2 env)  -- 085
evalExp (ExpSub e1 e2) env = arith (-) (evalExp e1 env) (evalExp e2 env)  -- 086
evalExp (ExpMul e1 e2) env = arith (*) (evalExp e1 env) (evalExp e2 env)  -- 087
evalExp (ExpDiv e1 e2) env = arith div (evalExp e1 env) (evalExp e2 env)  -- 088
                                                                          -- 089
evalExp (ExpIf e1 e2 e3) env = op1 (evalExp e1 env) f                     -- 090
  where f (ValInt 0) = evalExp e3 env                                     -- 091
        f (ValInt _) = evalExp e2 env                                     -- 092
        f _ = cerror "invalid value for 'if'"                             -- 093
                                                                          -- 094
evalExp (ExpApp e1 e2) env = op2 app (evalExp e1 env) (evalExp e2 env)    -- 095
  where app (ValFunc f) vp = f vp                                         -- 096
        app _ _ = cerror "attempt to call a non-function value"           -- 097
                                                                          -- 098
evalExp (ExpLambda v e) env = op0 (closure v (evalExp e) env)             -- 099
                                                                          -- 100
evalExp (ExpLet v v' e' e) env = evalExp e env'                           -- 101
  where env' = bind v (closure v' (evalExp e') env') env                  -- 102
                                                                          -- 103
                                                                          -- 104
---------------------------------------------------------------------------- 105
                                                                          -- 106
                                                                          -- 107
-------------------------------------------------------------------       -- 108
-- some examples                                                          -- 109
                                                                          -- 110
-- (34 + 52) or 0                                                         -- 111
exp1 = ExpIf (ExpAdd (ExpK 34) (ExpK 52)) (ExpK 43) (ExpK 4)              -- 112
                                                                          -- 113
f1 = ExpLambda "x" (ExpApp (ExpVar "x") (ExpVar "x"))                     -- 114
f2 = ExpApp f1 f1                                                         -- 115
                                                                          -- 116
f3 = ExpApp (ExpLambda "x" (ExpK 3)) f2                                   -- 117
                                                                          -- 118
                                                                          -- 119
fat4 = ExpLet "f"                                                         -- 120
              "x"                                                         -- 121
           (ExpIf (ExpVar "x")                                            -- 122
                  (ExpMul (ExpVar "x")                                    -- 123
                          (ExpApp (ExpVar "f")                            -- 124
                                  (ExpSub (ExpVar "x") (ExpK 1))))        -- 125
                  (ExpK 1))                                               -- 126
        (ExpApp (ExpVar "f") (ExpK 5))                                    -- 127
                                                                          -- 128
                                                                          -- 129
                                                                          -- 130
-- code to show the final value of an expression                          -- 131
main :: IO ()                                                             -- 132
main = print (evalExp fat4 emptyEnv k)                                    -- 133
  where k x = case x of                                                   -- 134
                ValInt i  -> show i                                       -- 135
                ValFunc _ -> "function"                                   -- 136
                                                                          -- 137