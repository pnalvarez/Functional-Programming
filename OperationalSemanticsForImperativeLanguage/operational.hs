---------------------------------------------------                       -- 001
-- Operational Semantics for an Imperative Language                       -- 002
---------------------------------------------------                       -- 003
                                                                          -- 004
-- variables are just names                                               -- 005
type Var = String                                                         -- 006
                                                                          -- 007
-- values are always integers (for now)                                   -- 008
type Value = Integer                                                      -- 009
                                                                          -- 010
-- a Memory maps variables to Values                                      -- 011
type Mem = Var -> Value                                                   -- 012
                                                                          -- 013
-- An empty memory                                                        -- 014
emptyMem :: Mem                                                           -- 015
emptyMem v = error ("invalid access to variable '" ++ v ++ "'")           -- 016
                                                                          -- 017
-- update the value of a variable in a memory                             -- 018
update :: Var -> Value -> Mem -> Mem                                      -- 019
update var val m = \v -> if v == var then val else m v                    -- 020
                                                                          -- 021
--------------------------------------------------------------------      -- 022
-- Abstract Syntax Tree for Expressions                                   -- 023
data Exp = ExpK Integer          -- constants                             -- 024
         | ExpVar Var            -- variables                             -- 025
         | ExpAdd Exp Exp        -- e1 + e2                               -- 026
         | ExpSub Exp Exp        -- e1 - e2                               -- 027
         | ExpMul Exp Exp        -- e1 * e2                               -- 028
         | ExpDiv Exp Exp        -- e1 / e2                               -- 029
         | ExpNeg Exp            -- -e                                    -- 030
                                                                          -- 031
-- Evaluates an expression in a given memory                              -- 032
evalExp :: Exp -> Mem -> Value                                            -- 033
                                                                          -- 034
evalExp (ExpK i) m = i                                                    -- 035
evalExp (ExpVar v) m = m v                                                -- 036
evalExp (ExpAdd e1 e2) m = (evalExp e1 m) + (evalExp e2 m)                -- 037
evalExp (ExpSub e1 e2) m = (evalExp e1 m) - (evalExp e2 m)                -- 038
evalExp (ExpMul e1 e2) m = (evalExp e1 m) * (evalExp e2 m)                -- 039
evalExp (ExpDiv e1 e2) m = (evalExp e1 m)  `div` (evalExp e2 m)           -- 040
evalExp (ExpNeg e) m = -(evalExp e m)                                     -- 041
                                                                          -- 042
                                                                          -- 043
----------------------------------------------------------------------    -- 044
                                                                          -- 045
-- Abstract Syntax Tree for Statements (commands)                         -- 046
data Cmd = CmdAsg Var Exp            -- assignment (var = exp)            -- 047
         | CmdIf Exp Cmd Cmd         -- if exp then c1 else c2            -- 048
         | CmdSeq Cmd Cmd            -- c1; c2                            -- 049
         | CmdWhile Exp Cmd          -- while e do c                      -- 050
         | CmdSkip                   -- do nothing                        -- 051
                                                                          -- 052
                                                                          -- 053
step :: Cmd -> Mem -> (Cmd, Mem)                                          -- 054
                                                                          -- 055
step (CmdSeq CmdSkip c) m = (c, m)                                        -- 056
                                                                          -- 057
step (CmdSeq c1 c2) m = (CmdSeq c1' c2, m')                               -- 058
  where (c1', m') = step c1 m                                             -- 059
                                                                          -- 060
step (CmdIf e c1 c2) m = if isTrue (evalExp e m) then (c1, m) else (c2, m)-- 061
  where isTrue 0 = False                                                  -- 062
        isTrue _ = True                                                   -- 063
                                                                          -- 064
step (CmdWhile e c) m = (CmdIf e (CmdSeq c (CmdWhile e c)) CmdSkip, m)    -- 065
                                                                          -- 066
step (CmdAsg v e) m = (CmdSkip, update v (evalExp e m) m)                 -- 067
                                                                          -- 068
                                                                          -- 069
evalCmd :: Cmd -> Mem -> Mem                                              -- 070
evalCmd CmdSkip m = m                                                     -- 071
evalCmd c m = evalCmd c' m'                                               -- 072
  where (c', m') = step c m                                               -- 073
                                                                          -- 074
                                                                          -- 075
-------------------------------------------------------------------       -- 076
-------------------------------------------------------------------       -- 077
-- example                                                                -- 078
                                                                          -- 079
-- y = 10; x = 1; while y do  x = x * y; y = y - 1                        -- 080
cmd1 = CmdSeq (CmdSeq (CmdAsg "y" (ExpK 10))                              -- 081
                      (CmdAsg "x" (ExpK 1)))                              -- 082
              (CmdWhile (ExpVar "y")                                      -- 083
                        (CmdSeq (CmdAsg "x" (ExpMul (ExpVar "x") (ExpVar "y")))-- 084
                                (CmdAsg "y" (ExpSub (ExpVar "y") (ExpK 1)))))-- 085
                                                                          -- 086
                                                                          -- 087
-------------------------------------------------------------------       -- 088
-- code to show the final value of "x" after running "cmd1" on            -- 089
-- an initially empty memory                                              -- 090
                                                                          -- 091
finalMem = evalCmd cmd1 emptyMem                                          -- 092
                                                                          -- 093
main = print (finalMem "x")                                               -- 094
                                                                          -- 095