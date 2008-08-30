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
import HST.CSPM.Values
import HST.CSPM.Environments
import HST.CSPM.BoundExpressions

evalAsNumber :: BoundExpression -> Int
evalAsNumber = coerceNumber . eval

evalAsSequence :: BoundExpression -> [Value]
evalAsSequence = coerceSequence . eval

evalAsSet :: BoundExpression -> [Value]
evalAsSet = coerceSet . eval

evalAsBoolean :: BoundExpression -> Bool
evalAsBoolean = coerceBoolean . eval

evalAsTuple :: BoundExpression -> [Value]
evalAsTuple = coerceTuple . eval

eval :: BoundExpression -> Value

eval BBottom = VBottom

-- Expressions that evaluate to a number

eval (BNLit i)         = VNumber $ i
eval (BNNeg m)         = VNumber $ -(evalAsNumber m)
eval (BNSum m n)       = VNumber $ (evalAsNumber m) + (evalAsNumber n)
eval (BNDiff m n)      = VNumber $ (evalAsNumber m) - (evalAsNumber n)
eval (BNProd m n)      = VNumber $ (evalAsNumber m) * (evalAsNumber n)
eval (BNQuot m n)      = VNumber $ (evalAsNumber m) `quot` (evalAsNumber n)
eval (BNRem m n)       = VNumber $ (evalAsNumber m) `rem` (evalAsNumber n)
eval (BQLength s)      = VNumber $ length (evalAsSequence s)
eval (BSCardinality a) = VNumber $ length (evalAsSet a)

-- Expressions that evaluate to a sequence

eval (BQLit xs)          = VSequence $ map eval xs
eval (BQClosedRange m n) = VSequence $
                           map VNumber
                           [evalAsNumber m .. evalAsNumber n]
eval (BQOpenRange m)     = VSequence $
                           map VNumber
                           [evalAsNumber m .. ]
eval (BQConcat s t)      = VSequence $
                           (evalAsSequence s) ++ (evalAsSequence t)
eval (BQTail s)          = VSequence $ tail (evalAsSequence s)

-- Expressions that evaluate to a set

eval (BSLit xs)              = VSet $ nub (map eval xs)
eval (BSClosedRange m n)     = VSet $ map VNumber
                               [evalAsNumber m .. evalAsNumber n]
eval (BSOpenRange m)         = VSet $ map VNumber [evalAsNumber m ..]
eval (BSUnion a b)           = VSet $ (evalAsSet a) `union` (evalAsSet b)
eval (BSIntersection a b)    = VSet $ (evalAsSet a) `intersect` (evalAsSet b)
eval (BSDifference a b)      = VSet $ (evalAsSet a) \\ (evalAsSet b)
eval (BSDistUnion aa)        = VSet $ distUnion
                               (map coerceSet (evalAsSet aa))
eval (BSDistIntersection aa) = VSet $ distIntersect
                               (map coerceSet (evalAsSet aa))
eval (BQSet s)               = VSet $ nub (evalAsSequence s)
eval (BSPowerset a)          = VSet $ map VSet (powerset (evalAsSet a))
eval (BSSequenceset a)       = VSet $ map VSequence (sequenceset (evalAsSet a))

-- Expressions that evaluate to a boolean

eval BBTrue          = VBoolean $ True
eval BBFalse         = VBoolean $ False
eval (BBAnd b1 b2)   = VBoolean $ (evalAsBoolean b1) && (evalAsBoolean b2)
eval (BBOr b1 b2)    = VBoolean $ (evalAsBoolean b1) || (evalAsBoolean b2)
eval (BBNot b)       = VBoolean $ not (evalAsBoolean b)
eval (BEqual x y)    = VBoolean $ (eval x) == (eval y)
eval (BNotEqual x y) = VBoolean $ (eval x) /= (eval y)
eval (BLT x y)       = VBoolean $ (eval x) < (eval y)
eval (BGT x y)       = VBoolean $ (eval x) > (eval y)
eval (BLTE x y)      = VBoolean $ (eval x) <= (eval y)
eval (BGTE x y)      = VBoolean $ (eval x) >= (eval y)
eval (BQEmpty s)     = VBoolean $ null (evalAsSequence s)
eval (BQIn x s)      = VBoolean $ (eval x) `elem` (evalAsSequence s)
eval (BSIn x a)      = VBoolean $ (eval x) `elem` (evalAsSet a)
eval (BSEmpty a)     = VBoolean $ null (evalAsSet a)

-- Expressions that evaluate to a tuple

eval (BTLit xs) = VTuple $ map eval xs

-- Expressions that can evaluate to anything

eval (BQHead s) = head (evalAsSequence s)

eval (BIfThenElse b x y) = if (evalAsBoolean b) then
                               eval x
                           else
                               eval y

eval (BVar e id) = eval $ bind e $ lookupExpr e id
