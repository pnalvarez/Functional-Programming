------------------------------------------------------                    -- 001
-- Functional language with side effects monadic-style                    -- 002
------------------------------------------------------                    -- 003
                                                                          -- 004
-- variables are just names                                               -- 005
type Var = String                                                         -- 006
                                                                          -- 007
                                                                          -- 008
-- locations are just numbers (addresses)                                 -- 009
type Location = Int                                                       -- 010
                                                                          -- 011
                                                                          -- 012
-- values are integers and functions                                      -- 013
data Value = ValInt Integer                                               -- 014
           | ValFunc (Value -> Cmpt Value)                                -- 015
           | ValError                                                     -- 016
                                                                          -- 017
                                                                          -- 018
-- an Environment maps variables to Locations                             -- 019
type Env = [(Var, Location)]                                              -- 020
                                                                          -- 021
                                                                          -- 022
-- a Memory maps locations to Values                                      -- 023
type Mem = [(Location, Value)]                                            -- 024
                                                                          -- 025
                                                                          -- 026
-- auxiliary function to map Values to Booleans                           -- 027
isTrue :: Value -> Bool                                                   -- 028
isTrue (ValInt i) = (i /= 0)                                              -- 029
                                                                          -- 030
                                                                          -- 031
-- the "side-effect" of an expression                                     -- 032
type Cmpt a = Mem -> (a, Mem)                                             -- 033
                                                                          -- 034
                                                                          -- 035
-- transforms a value into a computation                                  -- 036
op0 :: a -> Cmpt a                                                        -- 037
op0 v = \m -> (v, m)                                                      -- 038
                                                                          -- 039
-- executes an unary operation on computations                            -- 040
op1 :: Cmpt a -> (a -> Cmpt b) -> Cmpt b                                  -- 041
op1 ca op m = op v m'                                                     -- 042
  where (v, m') = ca m                                                    -- 043
                                                                          -- 044
cerror _ = op0 ValError                                                   -- 045
                                                                          -- 046
                                                                          -- 047
query :: Eq a => [(a, b)] -> a -> b                                       -- 048
query ((y, v):l) x | (x == y) = v                                         -- 049
query (_:l) x | otherwise = query l x                                     -- 050
                                                                          -- 051
update :: a -> b -> [(a, b)] -> [(a, b)]                                  -- 052
update k v m = (k,v):m                                                    -- 053
                                                                          -- 054
                                                                          -- 055
queryVar :: Location -> Cmpt Value                                        -- 056
queryVar loc m = (query m loc, m)                                         -- 057
                                                                          -- 058
                                                                          -- 059
updateVar :: Location -> Cmpt Value -> Cmpt Value                         -- 060
updateVar loc c1 = op1 c1 f                                               -- 061
  where f v = \m -> (v, update loc v m)                                   -- 062
                                                                          -- 063
                                                                          -- 064
-- find a free location in a memory                                       -- 065
freeM :: Cmpt Location                                                    -- 066
freeM = \m -> (free m, m)                                                 -- 067
                                                                          -- 068
free :: Mem -> Location                                                   -- 069
free [] = 1                                                               -- 070
free ((l,v):m') = (max l (free m')) + 1                                   -- 071
                                                                          -- 072
                                                                          -- 073
-- bind a new value in a new location                                     -- 074
bind :: Var -> Value -> Env -> Cmpt Env                                   -- 075
bind var val env = op1 freeM f                                            -- 076
  where f l = op1 (updateVar l (op0 val)) (\_ -> op0 (update var l env))  -- 077
                                                                          -- 078
                                                                          -- 079
-- executes a binary operation on computations                            -- 080
op2 :: (a -> b -> Cmpt c) -> Cmpt a -> Cmpt b -> Cmpt c                   -- 081
op2 op ca cb = op1 ca (\a -> op1 cb (op a))                               -- 082
                                                                          -- 083
-- executes a binary integer operation on computations                    -- 084
arith :: (Integer -> Integer -> Integer) ->                               -- 085
             Cmpt Value -> Cmpt Value -> Cmpt Value                       -- 086
arith op = op2 op_aux                                                     -- 087
  where op_aux (ValInt i1) (ValInt i2) = op0 (ValInt (op i1 i2))          -- 088
        op_aux _ _ = cerror "binary operation over non-int value"         -- 089
                                                                          -- 090
                                                                          -- 091
--------------------------------------------------------------------      -- 092
-- Abstract Syntax Tree for Expressions                                   -- 093
data Exp = ExpK Integer          -- constants                             -- 094
         | ExpVar Var            -- variables                             -- 095
         | ExpAssg Var Exp       -- v := e                                -- 096
         | ExpAdd Exp Exp        -- e1 + e2                               -- 097
         | ExpSub Exp Exp        -- e1 - e2                               -- 098
         | ExpMul Exp Exp        -- e1 * e2                               -- 099
         | ExpDiv Exp Exp        -- e1 / e2                               -- 100
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 -- 101
         | ExpApp Exp Exp        -- e1 e2                                 -- 102
         | ExpLambda Var Exp     -- \x -> e                               -- 103
         | ExpLet Var Var Exp Exp    -- letrec x=e1 in e2                 -- 104
                                                                          -- 105
                                                                          -- 106
closure :: Var -> Exp -> Env -> Value                                     -- 107
closure v e env = ValFunc f                                               -- 108
  where f x = op1 (bind v x env) (\env' -> evalExp e env')                -- 109
                                                                          -- 110
                                                                          -- 111
-- Evaluates an expression in a given environment                         -- 112
evalExp :: Exp -> Env -> Cmpt Value                                       -- 113
                                                                          -- 114
evalExp (ExpK i) env = op0 (ValInt i)                                     -- 115
evalExp (ExpVar var) env = queryVar (query env var)                       -- 116
                                                                          -- 117
evalExp (ExpAssg var e) env = updateVar (query env var) (evalExp e env)   -- 118
                                                                          -- 119
evalExp (ExpAdd e1 e2) env = arith (+) (evalExp e1 env) (evalExp e2 env)  -- 120
evalExp (ExpSub e1 e2) env = arith (-) (evalExp e1 env) (evalExp e2 env)  -- 121
evalExp (ExpMul e1 e2) env = arith (*) (evalExp e1 env) (evalExp e2 env)  -- 122
evalExp (ExpDiv e1 e2) env = arith div (evalExp e1 env) (evalExp e2 env)  -- 123
                                                                          -- 124
evalExp (ExpIf e1 e2 e3) env = op1 (evalExp e1 env) f                     -- 125
  where f (ValInt 0) = evalExp e3 env                                     -- 126
        f (ValInt _) = evalExp e2 env                                     -- 127
        f _ = cerror "invalid value for 'if'"                             -- 128
                                                                          -- 129
evalExp (ExpApp e1 e2) env = op2 app (evalExp e1 env) (evalExp e2 env)    -- 130
  where app (ValFunc f) vp = f vp                                         -- 131
        app _ _ = cerror "attempt to call a non-function value"           -- 132
                                                                          -- 133
evalExp (ExpLambda v e) env = op0 (closure v e env)                       -- 134
                                                                          -- 135
evalExp (ExpLet v v' e' e) env = op1 freeM f                              -- 136
  where f l = let env' = update v l env;                                  -- 137
                  c = updateVar l (op0 (closure v' e' env')) in           -- 138
                op1 c (\_ -> evalExp e env')                              -- 139
                                                                          -- 140
---------------------------------------------------------------------------- 141
-------------------------------------------------------------------       -- 142
-- examples                                                               -- 143
                                                                          -- 144
comma :: Exp -> Exp -> Exp                                                -- 145
comma e1 e2 = ExpApp (ExpLambda "_" e2) e1                                -- 146
                                                                          -- 147
                                                                          -- 148
-- (\x -> (x := x + 10); x) 15                                            -- 149
t1 = ExpApp                                                               -- 150
       (ExpLambda "x"                                                     -- 151
           ((ExpAssg "x" (ExpAdd (ExpVar "x") (ExpK 10)))                 -- 152
               `comma`                                                    -- 153
            (ExpVar "x")))                                                -- 154
       (ExpK 15)                                                          -- 155
                                                                          -- 156
                                                                          -- 157
-- letrec f = (\x -> if x then x * f(x - 1) else 1) in f 10               -- 158
t2 = ExpLet "f" "x"                                                       -- 159
      (ExpIf (ExpVar "x")                                                 -- 160
             (ExpMul (ExpVar "x")                                         -- 161
                     (ExpApp (ExpVar "f") (ExpSub (ExpVar "x") (ExpK 1))))-- 162
             (ExpK 1))                                                    -- 163
      (ExpApp (ExpVar "f") (ExpK 10))                                     -- 164
                                                                          -- 165
-- code to show the final value of an expression                          -- 166
main :: IO ()                                                             -- 167
main = print ((case (evalExp t2 [] []) of                                 -- 168
                         (ValInt i, _) -> i                               -- 169
                         _        -> -1))                                 -- 170
                                                                          -- 171