-----------------------------------------------------------------------
--
--  Copyright © 2008 Douglas Creager
--
--    This library is free software; you can redistribute it and/or
--    modify it under the terms of the GNU Lesser General Public
--    License as published by the Free Software Foundation; either
--    version 2.1 of the License, or (at your option) any later
--    version.
--
--    This library is distributed in the hope that it will be useful,
--    but WITHOUT ANY WARRANTY; without even the implied warranty of
--    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
--    GNU Lesser General Public License for more details.
--
--    You should have received a copy of the GNU Lesser General Public
--    License along with this library; if not, write to the Free
--    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
--    MA 02111-1307 USA
--
------------------------------------------------------------------------

module HST.CSPM.Evaluate where

import Data.List

import HST.CSPM.Utils
import HST.CSPM.Expressions
import HST.CSPM.Values

evalAsNumber :: Expression -> Int
evalAsNumber = coerceNumber . eval

evalAsSequence :: Expression -> [Value]
evalAsSequence = coerceSequence . eval

evalAsSet :: Expression -> [Value]
evalAsSet = coerceSet . eval

evalAsBoolean :: Expression -> Bool
evalAsBoolean = coerceBoolean . eval

evalAsTuple :: Expression -> [Value]
evalAsTuple = coerceTuple . eval

eval :: Expression -> Value

eval EBottom = VBottom

-- Expressions that evaluate to a number

eval (ENLit i)         = VNumber $ i
eval (ENNeg m)         = VNumber $ -(evalAsNumber m)
eval (ENSum m n)       = VNumber $ (evalAsNumber m) + (evalAsNumber n)
eval (ENDiff m n)      = VNumber $ (evalAsNumber m) - (evalAsNumber n)
eval (ENProd m n)      = VNumber $ (evalAsNumber m) * (evalAsNumber n)
eval (ENQuot m n)      = VNumber $ (evalAsNumber m) `quot` (evalAsNumber n)
eval (ENRem m n)       = VNumber $ (evalAsNumber m) `rem` (evalAsNumber n)
eval (EQLength s)      = VNumber $ length (evalAsSequence s)
eval (ESCardinality a) = VNumber $ length (evalAsSet a)

-- Expressions that evaluate to a sequence

eval (EQLit xs)          = VSequence $ map eval xs
eval (EQClosedRange m n) = VSequence $
                           map VNumber
                           [evalAsNumber m .. evalAsNumber n]
eval (EQOpenRange m)     = VSequence $
                           map VNumber
                           [evalAsNumber m .. ]
eval (EQConcat s t)      = VSequence $
                           (evalAsSequence s) ++ (evalAsSequence t)
eval (EQTail s)          = VSequence $ tail (evalAsSequence s)

-- Expressions that evaluate to a set

eval (ESLit xs)              = VSet $ nub (map eval xs)
eval (ESClosedRange m n)     = VSet $ map VNumber
                               [evalAsNumber m .. evalAsNumber n]
eval (ESOpenRange m)         = VSet $ map VNumber [evalAsNumber m ..]
eval (ESUnion a b)           = VSet $ (evalAsSet a) `union` (evalAsSet b)
eval (ESIntersection a b)    = VSet $ (evalAsSet a) `intersect` (evalAsSet b)
eval (ESDifference a b)      = VSet $ (evalAsSet a) \\ (evalAsSet b)
eval (ESDistUnion aa)        = VSet $ distUnion
                               (map coerceSet (evalAsSet aa))
eval (ESDistIntersection aa) = VSet $ distIntersect
                               (map coerceSet (evalAsSet aa))
eval (EQSet s)               = VSet $ nub (evalAsSequence s)
eval (ESPowerset a)          = VSet $ map VSet (powerset (evalAsSet a))
eval (ESSequenceset a)       = VSet $ map VSequence (sequenceset (evalAsSet a))

-- Expressions that evaluate to a boolean

eval EBTrue          = VBoolean $ True
eval EBFalse         = VBoolean $ False
eval (EBAnd b1 b2)   = VBoolean $ (evalAsBoolean b1) && (evalAsBoolean b2)
eval (EBOr b1 b2)    = VBoolean $ (evalAsBoolean b1) || (evalAsBoolean b2)
eval (EBNot b)       = VBoolean $ not (evalAsBoolean b)
eval (EEqual x y)    = VBoolean $ (eval x) == (eval y)
eval (ENotEqual x y) = VBoolean $ (eval x) /= (eval y)
eval (ELT x y)       = VBoolean $ (eval x) < (eval y)
eval (EGT x y)       = VBoolean $ (eval x) > (eval y)
eval (ELTE x y)      = VBoolean $ (eval x) <= (eval y)
eval (EGTE x y)      = VBoolean $ (eval x) >= (eval y)
eval (EQEmpty s)     = VBoolean $ null (evalAsSequence s)
eval (EQIn x s)      = VBoolean $ (eval x) `elem` (evalAsSequence s)
eval (ESIn x a)      = VBoolean $ (eval x) `elem` (evalAsSet a)
eval (ESEmpty a)     = VBoolean $ null (evalAsSet a)

-- Expressions that evaluate to a tuple

eval (ETLit xs) = VTuple $ map eval xs

-- Expressions that can evaluate to anything

eval (EQHead s) = head (evalAsSequence s)

eval (EIfThenElse b x y) = if (evalAsBoolean b) then
                               eval x
                           else
                               eval y
