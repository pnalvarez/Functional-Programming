--------------------------------                                          -- 001
-- Early Functional Language CPS                                          -- 002
--------------------------------                                          -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
-- values are integers and functions                                      -- 009
data Value = ValInt Int                                                   -- 010
           | ValFunc (Value -> Cmpt)                                      -- 011
                                                                          -- 012
-- final results are strings (to be printed)                              -- 013
type Result = String                                                      -- 014
                                                                          -- 015
                                                                          -- 016
-- continuations                                                          -- 017
type K = Value -> Result                                                  -- 018
                                                                          -- 019
-- a computation gets a continuation and gives a result                   -- 020
type Cmpt = K -> Result                                                   -- 021
                                                                          -- 022
-- an Environment maps variables to Values through continuations          -- 023
type Env = Var -> Cmpt                                                    -- 024
                                                                          -- 025
                                                                          -- 026
-- auxiliary function to map Values to Booleans                           -- 027
isTrue :: Value -> Bool                                                   -- 028
isTrue (ValInt i) = (i /= 0)                                              -- 029
                                                                          -- 030
                                                                          -- 031
-- An empty Environment                                                   -- 032
emptyEnv :: Env                                                           -- 033
emptyEnv v k = ("undefined variable " ++ v)                               -- 034
                                                                          -- 035
                                                                          -- 036
-- bind a new value in an environment                                     -- 037
bind :: Var -> Value -> Env -> Env                                        -- 038
bind var val env = \v k -> if (var == v) then (k val) else (env v k)      -- 039
                                                                          -- 040
                                                                          -- 041
-- executes a binary operation on computations                            -- 042
up2 :: (Value -> Value -> Cmpt) -> (Cmpt -> Cmpt -> Cmpt)                 -- 043
up2 op c1 c2 k = c1 (\v1 -> c2 (\v2 -> op v1 v2 k))                       -- 044
                                                                          -- 045
-- executes an integer operation on values                                -- 046
arith :: (Int -> Int -> Int) -> (Value -> Value -> Cmpt)                  -- 047
arith op (ValInt i1) (ValInt i2) k = k (ValInt (op i1 i2))                -- 048
arith _ _ _ _ = "binary operation over non-int value"                     -- 049
                                                                          -- 050
                                                                          -- 051
-- executes an integer operation on computations                          -- 052
binOp :: (Int -> Int -> Int) -> (Cmpt -> Cmpt -> Cmpt)                    -- 053
binOp op = up2 (arith op)                                                 -- 054
                                                                          -- 055
                                                                          -- 056
----------------------------------------------------------------------    -- 057
-- Abstract Syntax Tree for Expressions                                   -- 058
data Exp = ExpK Int              -- constants                             -- 059
         | ExpVar Var            -- variables                             -- 060
         | ExpAdd Exp Exp        -- e1 + e2                               -- 061
         | ExpSub Exp Exp        -- e1 - e2                               -- 062
         | ExpMul Exp Exp        -- e1 * e2                               -- 063
         | ExpDiv Exp Exp        -- e1 / e2                               -- 064
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 -- 065
         | ExpApp Exp Exp        -- e1 e2                                 -- 066
         | ExpLambda Var Exp     -- \x -> e                               -- 067
         | ExpLet Var Var Exp Exp        -- letrec x=(\x'->e') in e       -- 068
                                                                          -- 069
-- creates a closure in given environment                                 -- 070
closure :: Var -> (Env -> Cmpt) -> Env -> Value                           -- 071
closure v e env = ValFunc (\x k' -> e (bind v x env) k')                  -- 072
                                                                          -- 073
-- Evaluates an expression in a given environment                         -- 074
-- with a given continuation                                              -- 075
evalExp :: Exp -> Env -> Cmpt                                             -- 076
                                                                          -- 077
evalExp (ExpK i) env k = k (ValInt i)                                     -- 078
evalExp (ExpVar v) env k = env v k                                        -- 079
evalExp (ExpAdd e1 e2) env k =                                            -- 080
            binOp (+) (evalExp e1 env) (evalExp e2 env) k                 -- 081
evalExp (ExpSub e1 e2) env k =                                            -- 082
            binOp (-) (evalExp e1 env) (evalExp e2 env) k                 -- 083
evalExp (ExpMul e1 e2) env k =                                            -- 084
            binOp (*) (evalExp e1 env) (evalExp e2 env) k                 -- 085
evalExp (ExpDiv e1 e2) env k =                                            -- 086
            binOp div (evalExp e1 env) (evalExp e2 env) k                 -- 087
                                                                          -- 088
evalExp (ExpIf e1 e2 e3) env k = evalExp e1 env k'                        -- 089
  where k' (ValInt 0) = evalExp e3 env k                                  -- 090
        k' (ValInt _) = evalExp e2 env k                                  -- 091
        k' _ = "invalid value for 'if'"                                   -- 092
                                                                          -- 093
evalExp (ExpApp e1 e2) env k = up2 app (evalExp e1 env) (evalExp e2 env) k-- 094
  where app (ValFunc f) v2 k = f v2 k                                     -- 095
        app _ _ _ = "attempt to call a non-function value"                -- 096
                                                                          -- 097
evalExp (ExpLambda v e) env k = k (closure v (evalExp e) env)             -- 098
                                                                          -- 099
evalExp (ExpLet v v' e' e) env k = evalExp e env' k                       -- 100
  where env' = bind v (closure v' (evalExp e') env') env                  -- 101
                                                                          -- 102
                                                                          -- 103
----------------------------------------------------------------------    -- 104
                                                                          -- 105
                                                                          -- 106
----------------------------------------------------------------------    -- 107
-- some examples                                                          -- 108
                                                                          -- 109
-- (34 + 52) or 0                                                         -- 110
exp1 = ExpIf (ExpAdd (ExpK 34) (ExpK 52)) (ExpK 43) (ExpK 4)              -- 111
                                                                          -- 112
f1 = ExpLambda "x" (ExpApp (ExpVar "x") (ExpVar "x"))                     -- 113
f2 = ExpApp f1 f1                                                         -- 114
                                                                          -- 115
f3 = ExpApp (ExpLambda "x" (ExpK 3)) f2                                   -- 116
                                                                          -- 117
fat4 = ExpLet "f"                                                         -- 118
              "x"                                                         -- 119
           (ExpIf (ExpVar "x")                                            -- 120
                  (ExpMul (ExpVar "x")                                    -- 121
                          (ExpApp (ExpVar "f")                            -- 122
                                  (ExpSub (ExpVar "x") (ExpK 1))))        -- 123
                  (ExpK 1))                                               -- 124
        (ExpApp (ExpVar "f") (ExpK 5))                                    -- 125
                                                                          -- 126
                                                                          -- 127
                                                                          -- 128
-- code to show the final value of an expression                          -- 129
main :: IO ()                                                             -- 130
main = print (evalExp fat4 emptyEnv k)                                    -- 131
  where k x = case x of                                                   -- 132
                ValInt i  -> show i                                       -- 133
                ValFunc _ -> "function"                                   -- 134
                                                                          -- 135