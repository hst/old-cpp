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

import HST.CSPM.Expressions
import HST.CSPM.Values

eval :: Expression -> Value

eval EBottom             = VBottom
eval (ENumber id)        = VNumber (evalNumber id)
eval (ESequence s)       = VSequence (evalSequence s)
eval (ESet a)            = VSet (evalSet a)
eval (EBoolean b)        = VBoolean (evalBoolean b)
eval (ETuple t)          = VTuple (evalTuple t)
eval (QHead s)           = head (evalAsSequence s)
eval (EIfThenElse b x y) = if (evalAsBoolean b) then
                               eval x
                           else
                               eval y

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


evalNumber :: Number -> Int

evalNumber (NLit i)         = i
evalNumber (NNeg m)         = -(evalAsNumber m)
evalNumber (NSum m n)       = (evalAsNumber m) + (evalAsNumber n)
evalNumber (NDiff m n)      = (evalAsNumber m) - (evalAsNumber n)
evalNumber (NProd m n)      = (evalAsNumber m) * (evalAsNumber n)
evalNumber (NQuot m n)      = (evalAsNumber m) `quot` (evalAsNumber n)
evalNumber (NRem m n)       = (evalAsNumber m) `rem` (evalAsNumber n)
evalNumber (QLength s)      = length (evalAsSequence s)
evalNumber (SCardinality a) = length (evalAsSet a)


evalSequence :: Sequence -> [Value]

evalSequence (QLit xs)          = map eval xs
evalSequence (QClosedRange m n) = map VNumber
                                      [evalAsNumber m .. evalAsNumber n]
evalSequence (QOpenRange m)     = map VNumber
                                      [evalAsNumber m .. ]
evalSequence (QConcat s t)      = (evalAsSequence s) ++ (evalAsSequence t)
evalSequence (QTail s)          = tail (evalAsSequence s)


distUnion :: Eq a => [[a]] -> [a]
distUnion xss = foldr union [] xss

distIntersect :: Eq a => [[a]] -> [a]
distIntersect []  = []
distIntersect xss = foldr1 intersect xss

powerset :: [a] -> [[a]]

powerset []     = [[]]
powerset (x:xs) = ps ++ map (x:) ps
                  where
                    ps = powerset xs

sequenceset :: [a] -> [[a]]
sequenceset as = [[]] ++
                 concat (map (\xs -> (map (:xs) as)) (sequenceset as))

evalSet :: Set -> [Value]

evalSet (SLit xs)              = nub (map eval xs)
evalSet (SClosedRange m n)     = map VNumber
                                 [evalAsNumber m .. evalAsNumber n]
evalSet (SOpenRange m)         = map VNumber [evalAsNumber m ..]
evalSet (SUnion a b)           = (evalAsSet a) `union` (evalAsSet b)
evalSet (SIntersection a b)    = (evalAsSet a) `intersect` (evalAsSet b)
evalSet (SDifference a b)      = (evalAsSet a) \\ (evalAsSet b)
evalSet (SDistUnion aa)        = distUnion
                                 (map coerceSet (evalAsSet aa))
evalSet (SDistIntersection aa) = distIntersect
                                 (map coerceSet (evalAsSet aa))
evalSet (QSet s)               = nub (evalAsSequence s)
evalSet (SPowerset a)          = map VSet (powerset (evalAsSet a))
evalSet (SSequenceset a)       = map VSequence (sequenceset (evalAsSet a))


evalBoolean :: Boolean -> Bool

evalBoolean BTrue           = True
evalBoolean BFalse          = False
evalBoolean (BAnd b1 b2)    = (evalAsBoolean b1) && (evalAsBoolean b2)
evalBoolean (BOr b1 b2)     = (evalAsBoolean b1) || (evalAsBoolean b2)
evalBoolean (BNot b)        = not (evalAsBoolean b)
evalBoolean (EEqual x y)    = (eval x) == (eval y)
evalBoolean (ENotEqual x y) = (eval x) /= (eval y)
evalBoolean (ELT x y)       = (eval x) < (eval y)
evalBoolean (EGT x y)       = (eval x) > (eval y)
evalBoolean (ELTE x y)      = (eval x) <= (eval y)
evalBoolean (EGTE x y)      = (eval x) >= (eval y)
evalBoolean (QEmpty s)      = null (evalAsSequence s)
evalBoolean (QIn x s)       = (eval x) `elem` (evalAsSequence s)
evalBoolean (SIn x a)       = (eval x) `elem` (evalAsSet a)
evalBoolean (SEmpty a)      = null (evalAsSet a)


evalTuple :: Tuple -> [Value]

evalTuple (TLit xs) = map eval xs
