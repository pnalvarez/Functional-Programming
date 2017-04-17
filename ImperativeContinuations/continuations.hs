--------------------------                                                -- 001
-- Imperative Language CPS                                                -- 002
--------------------------                                                -- 003
                                                                          -- 004
                                                                          -- 005
-- variables are just names                                               -- 006
type Var = String                                                         -- 007
                                                                          -- 008
-- values are always integers (for now)                                   -- 009
type Value = Int                                                          -- 010
                                                                          -- 011
-- a Memory maps variables to Values                                      -- 012
type Mem = Var -> Value                                                   -- 013
                                                                          -- 014
-- final result of a program                                              -- 015
type Result = String                                                      -- 016
                                                                          -- 017
                                                                          -- 018
-- a continuation finishes the execution of a program                     -- 019
type K = Mem -> Result                                                    -- 020
                                                                          -- 021
                                                                          -- 022
-- auxiliary function to map Values to Booleans                           -- 023
isTrue :: Value -> Bool                                                   -- 024
isTrue i = (i /= 0)                                                       -- 025
                                                                          -- 026
                                                                          -- 027
-- auxiliary function to map Values to Booleans                           -- 028
bool2Int :: Bool -> Value                                                 -- 029
bool2Int True = 1                                                         -- 030
bool2Int False = 0                                                        -- 031
                                                                          -- 032
                                                                          -- 033
-- An empty memory                                                        -- 034
emptyMem :: Mem                                                           -- 035
emptyMem v = 0                                                            -- 036
                                                                          -- 037
-- update the value of a variable in a memory                             -- 038
update :: Var -> Value -> Mem -> Mem                                      -- 039
update var val m = \v -> if var == v then val else m v                    -- 040
                                                                          -- 041
                                                                          -- 042
--------------------------------------------------------------------      -- 043
-- Abstract Syntax Tree for Expressions                                   -- 044
data Exp = ExpK Int              -- constants                             -- 045
         | ExpVar Var            -- variables                             -- 046
         | ExpAdd Exp Exp        -- e1 + e2                               -- 047
         | ExpSub Exp Exp        -- e1 - e2                               -- 048
         | ExpMul Exp Exp        -- e1 * e2                               -- 049
         | ExpDiv Exp Exp        -- e1 / e2                               -- 050
         | ExpAnd Exp Exp        -- e1 & e2                               -- 051
         | ExpOr Exp Exp         -- e1 | e2                               -- 052
         | ExpNot Exp            -- !e                                    -- 053
         | ExpNeg Exp            -- -e                                    -- 054
                                                                          -- 055
-- Evaluates an expression in a given memory                              -- 056
evalExp :: Exp -> Mem -> Value                                            -- 057
                                                                          -- 058
evalExp (ExpK i) m = i                                                    -- 059
evalExp (ExpVar v) m = m v                                                -- 060
evalExp (ExpAdd e1 e2) m = (evalExp e1 m) + (evalExp e2 m)                -- 061
evalExp (ExpSub e1 e2) m = (evalExp e1 m) - (evalExp e2 m)                -- 062
evalExp (ExpMul e1 e2) m = (evalExp e1 m) * (evalExp e2 m)                -- 063
evalExp (ExpDiv e1 e2) m = (evalExp e1 m)  `div` (evalExp e2 m)           -- 064
evalExp (ExpAnd e1 e2) m =                                                -- 065
    bool2Int (isTrue(evalExp e1 m)  && isTrue(evalExp e2 m))              -- 066
evalExp (ExpOr e1 e2) m =                                                 -- 067
    bool2Int(isTrue(evalExp e1 m)  || isTrue(evalExp e2 m))               -- 068
evalExp (ExpNeg e) m = -(evalExp e m)                                     -- 069
evalExp (ExpNot e) m = bool2Int(not (isTrue(evalExp e m)))                -- 070
                                                                          -- 071
                                                                          -- 072
----------------------------------------------------------------------    -- 073
-- Abstract Syntax Tree for Statements (commands)                         -- 074
data Cmd = CmdAss Var Exp            -- assignment (var = exp)            -- 075
         | CmdIf Exp Cmd Cmd         -- if exp then c1 else c2            -- 076
         | CmdComp Cmd Cmd           -- c1; c2                            -- 077
         | CmdWhile Exp Cmd          -- while e do c                      -- 078
         | CmdSkip                   -- do nothing                        -- 079
                                                                          -- 080
evalCmd :: Cmd -> K -> Mem -> Result                                      -- 081
                                                                          -- 082
evalCmd (CmdSkip) k m = k m                                               -- 083
evalCmd (CmdComp c1 c2) k m = evalCmd c1 (evalCmd c2 k) m                 -- 084
evalCmd (CmdIf e ct ce) k m =                                             -- 085
  if isTrue(evalExp e m)                                                  -- 086
    then (evalCmd ct k m) else (evalCmd ce k m)                           -- 087
evalCmd (CmdAss v e) k m = k (update v (evalExp e m) m)                   -- 088
evalCmd (CmdWhile e c) k m = k' m                                         -- 089
  where k' = \m -> (if isTrue(evalExp e m) then evalCmd c k' m            -- 090
                                           else k m)                      -- 091
                                                                          -- 092
                                                                          -- 093
-------------------------------------------------------------------       -- 094
-- some examples                                                          -- 095
                                                                          -- 096
-- (34 + 52) or 0                                                         -- 097
exp1 = ExpOr (ExpAdd (ExpK 34) (ExpK 52)) (ExpK 0)                        -- 098
                                                                          -- 099
-- y = 10; x = 0; while y do y = x - 1; x = x + 1                         -- 100
cmd1 = CmdComp                                                            -- 101
         (CmdAss "y" (ExpK 10))                                           -- 102
       (CmdComp                                                           -- 103
         (CmdAss "x" (ExpK 0))                                            -- 104
         (CmdWhile (ExpVar "y")                                           -- 105
                   (CmdComp                                               -- 106
                     (CmdAss "y" (ExpSub (ExpVar "y") (ExpK 1)))          -- 107
                     (CmdAss "x" (ExpAdd (ExpVar "x") (ExpK 1))))))       -- 108
                                                                          -- 109
                                                                          -- 110
-------------------------------------------------------------------       -- 111
-- code to show the final value of "x" after running "cmd1" on            -- 112
-- an initially empty memory                                              -- 113
                                                                          -- 114
-- initial continuation: get the value of "x" from the memory             -- 115
ik :: K                                                                   -- 116
ik m = show (m "x")                                                       -- 117
                                                                          -- 118
main :: IO ()                                                             -- 119
main = print (evalCmd cmd1 ik emptyMem)                                   -- 120
                                                                          -- 121