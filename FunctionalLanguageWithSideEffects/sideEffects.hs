---------------------------------------                                   -- 001
-- Funcional language with side effects                                   -- 002
---------------------------------------                                   -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
                                                                          -- 009
-- locations are just numbers (addresses)                                 -- 010
type Location = Int                                                       -- 011
                                                                          -- 012
                                                                          -- 013
-- values are integers and functions                                      -- 014
data Value = ValInt Integer                                               -- 015
           | ValFunc (Value -> SE)                                        -- 016
           | ValError                                                     -- 017
                                                                          -- 018
                                                                          -- 019
-- an Environment maps variables to Locations                             -- 020
type Env = [(Var, Location)]                                              -- 021
                                                                          -- 022
                                                                          -- 023
-- a Memory maps locations to Values                                      -- 024
type Mem = [(Location, Value)]                                            -- 025
                                                                          -- 026
                                                                          -- 027
-- auxiliary function to map Values to Booleans                           -- 028
isTrue :: Value -> Bool                                                   -- 029
isTrue (ValInt i) = (i /= 0)                                              -- 030
                                                                          -- 031
                                                                          -- 032
-- the "side-effect" of an expression                                     -- 033
type SE = Mem -> (Value, Mem)                                             -- 034
                                                                          -- 035
                                                                          -- 036
query :: Eq a => [(a, b)] -> a -> b                                       -- 037
query ((y, v):l) x | (x == y) = v                                         -- 038
query (_:l) x | otherwise = query l x                                     -- 039
                                                                          -- 040
update :: a -> b -> [(a, b)] -> [(a, b)]                                  -- 041
update k v m = (k,v):m                                                    -- 042
                                                                          -- 043
                                                                          -- 044
-- find a free location in a memory                                       -- 045
free :: Mem -> Location                                                   -- 046
free [] = 1                                                               -- 047
free ((l,v):m') = (max l (free m')) + 1                                   -- 048
                                                                          -- 049
                                                                          -- 050
-- bind a new value in a new location                                     -- 051
bind :: Var -> Value -> Env -> Mem -> (Env, Mem)                          -- 052
bind var val env m = (env', m')                                           -- 053
  where l = free m                                                        -- 054
        env' = update var l env                                           -- 055
        m' = update l val m                                               -- 056
                                                                          -- 057
                                                                          -- 058
                                                                          -- 059
-- executes a binary operation on values                                  -- 060
binOp :: (Integer -> Integer -> Integer) -> SE -> SE -> SE                -- 061
binOp op se1 se2 m = (binaux v1 v2, m'')                                  -- 062
  where (v1, m') = se1 m                                                  -- 063
        (v2, m'') = se2 m'                                                -- 064
        binaux (ValInt i1) (ValInt i2) = ValInt (op i1 i2)                -- 065
        binaux _ _ = ValError                                             -- 066
                                                                          -- 067
                                                                          -- 068
                                                                          -- 069
--------------------------------------------------------------------      -- 070
-- Abstract Syntax Tree for Expressions                                   -- 071
data Exp = ExpK Integer          -- constants                             -- 072
         | ExpVar Var            -- variables                             -- 073
         | ExpAss Var Exp        -- v := e                                -- 074
         | ExpAdd Exp Exp        -- e1 + e2                               -- 075
         | ExpSub Exp Exp        -- e1 - e2                               -- 076
         | ExpMul Exp Exp        -- e1 * e2                               -- 077
         | ExpDiv Exp Exp        -- e1 / e2                               -- 078
         | ExpIf Exp Exp Exp     -- if e1 then e2 else e3                 -- 079
         | ExpApp Exp Exp        -- e1 e2                                 -- 080
         | ExpLambda Var Exp     -- \x -> e                               -- 081
         | ExpLet Var Var Exp Exp    -- letrec x=e1 in e2                 -- 082
                                                                          -- 083
                                                                          -- 084
closure :: Var -> Exp -> Env -> Value                                     -- 085
closure v e env = ValFunc f                                               -- 086
  where f x m = evalExp e env' m'                                         -- 087
          where (env', m') = bind v x env m                               -- 088
                                                                          -- 089
                                                                          -- 090
-- Evaluates an expression in a given environment                         -- 091
evalExp :: Exp -> Env -> SE                                               -- 092
                                                                          -- 093
evalExp (ExpK i) env m = (ValInt i, m)                                    -- 094
evalExp (ExpVar var) env m = (query m (query env var), m)                 -- 095
                                                                          -- 096
evalExp (ExpAss var e) env m = (v1, update loc v1 m')                     -- 097
  where (v1, m') = evalExp e env m                                        -- 098
        loc = query env var                                               -- 099
                                                                          -- 100
evalExp (ExpAdd e1 e2) env m = binOp (+) (evalExp e1 env) (evalExp e2 env) m-- 101
evalExp (ExpSub e1 e2) env m = binOp (-) (evalExp e1 env) (evalExp e2 env) m-- 102
evalExp (ExpMul e1 e2) env m = binOp (*) (evalExp e1 env) (evalExp e2 env) m-- 103
evalExp (ExpDiv e1 e2) env m = binOp div (evalExp e1 env) (evalExp e2 env) m-- 104
                                                                          -- 105
evalExp (ExpIf e1 e2 e3) env m =                                          -- 106
    if isTrue v1 then evalExp e2 env m' else evalExp e3 env m'            -- 107
  where (v1, m') = evalExp e1 env m                                       -- 108
                                                                          -- 109
evalExp (ExpApp e1 e2) env m =                                            -- 110
  case v1 of                                                              -- 111
    ValFunc f -> f v2 m''                                                 -- 112
    _         -> (ValError, m'')                                          -- 113
  where (v1, m') = evalExp e1 env m                                       -- 114
        (v2, m'') = evalExp e2 env m'                                     -- 115
                                                                          -- 116
evalExp (ExpLambda v e) env m = (closure v e env, m)                      -- 117
                                                                          -- 118
evalExp (ExpLet v1 v2 e1 e2) env m = evalExp e2 env' m'                   -- 119
  where (env', m') = bind v1 (closure v2 e1 env') env m                   -- 120
                                                                          -- 121
                                                                          -- 122
---------------------------------------------------------------------------- 123
-------------------------------------------------------------------       -- 124
-- examples                                                               -- 125
                                                                          -- 126
comma :: Exp -> Exp -> Exp                                                -- 127
comma e1 e2 = ExpApp (ExpLambda "_" e2) e1                                -- 128
                                                                          -- 129
                                                                          -- 130
-- (\x -> (x := x + 10); x) 15                                            -- 131
t1 = ExpApp                                                               -- 132
       (ExpLambda "x"                                                     -- 133
           ((ExpAss "x" (ExpAdd (ExpVar "x") (ExpK 10))) `comma` (ExpVar "x")))-- 134
       (ExpK 15)                                                          -- 135
                                                                          -- 136
                                                                          -- 137
-- code to show the final value of an expression                          -- 138
main :: IO ()                                                             -- 139
main = print (case (evalExp t1 [] []) of                                  -- 140
                         (ValInt i, _) -> i                               -- 141
                         _        -> -1)                                  -- 142
                                                                          -- 143