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

module HST.CSPM.Evaluate (
                          eval, evalAsNumber, evalAsSequence,
                          evalAsSet, evalAsBoolean, evalAsTuple, run
                         ) where

import Control.Monad.State

import HST.CSPM.Sets (Set)
import qualified HST.CSPM.Sets as Sets
import HST.CSPM.Types
import HST.CSPM.Environments
import HST.CSPM.Bind

data EvalState
    = EvalState ()

emptyState :: EvalState
emptyState = EvalState ()

type Eval a = State EvalState a

run :: Eval a -> a
run = (flip evalState) emptyState

evalAsNumber :: BoundExpression -> Eval Int
evalAsNumber = (liftM coerceNumber) . eval

evalAsSequence :: BoundExpression -> Eval [Value]
evalAsSequence = (liftM coerceSequence) . eval

evalAsSet :: BoundExpression -> Eval (Set Value)
evalAsSet = (liftM coerceSet) . eval

evalAsBoolean :: BoundExpression -> Eval Bool
evalAsBoolean = (liftM coerceBoolean) . eval

evalAsTuple :: BoundExpression -> Eval [Value]
evalAsTuple = (liftM coerceTuple) . eval

eval :: BoundExpression -> Eval Value

eval BBottom = return VBottom

-- Expressions that evaluate to a number

eval (BNLit i) = return $ VNumber i

eval (BNNeg m) = do
  m' <- evalAsNumber m
  return $ VNumber $ negate m'

eval (BNSum m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VNumber $ m' + n'

eval (BNDiff m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VNumber $ m' - n'

eval (BNProd m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VNumber $ m' * n'

eval (BNQuot m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VNumber $ m' `quot` n'

eval (BNRem m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VNumber $ m' `rem` n'

eval (BQLength s) = do
  s' <- evalAsSequence s
  return $ VNumber $ length s'

eval (BSCardinality a) = do
  a' <- evalAsSet a
  return $ VNumber $ Sets.size a'

-- Expressions that evaluate to a sequence

eval (BQLit xs) = do
  xs' <- sequence $ map eval xs
  return $ VSequence xs'

eval (BQClosedRange m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VSequence $ map VNumber [m'..n']

eval (BQOpenRange m) = do
  m' <- evalAsNumber m
  return $ VSequence $ map VNumber [m'..]

eval (BQConcat s t) = do
  s' <- evalAsSequence s
  t' <- evalAsSequence t
  return $ VSequence $ s' ++ t'

eval (BQTail s) = do
  s' <- evalAsSequence s
  return $ VSequence $ tail s'

-- Expressions that evaluate to a set

eval (BSLit xs) = do
  xs' <- sequence $ map eval xs
  return $ VSet $ Sets.fromList xs'

eval (BSClosedRange m n) = do
  m' <- evalAsNumber m
  n' <- evalAsNumber n
  return $ VSet $ Sets.fromList $ map VNumber [m'..n']

eval (BSOpenRange m) = do
  m' <- evalAsNumber m
  return $ VSet $ Sets.fromList $ map VNumber [m'..]

eval (BSUnion a b) = do
  a' <- evalAsSet a
  b' <- evalAsSet b
  return $ VSet $ a' `Sets.union` b'

eval (BSIntersection a b) = do
  a' <- evalAsSet a
  b' <- evalAsSet b
  return $ VSet $ a' `Sets.intersect` b'

eval (BSDifference a b) = do
  a' <- evalAsSet a
  b' <- evalAsSet b
  return $ VSet $ a' `Sets.difference` b'

eval (BSDistUnion aa) = do
  aa' <- evalAsSet aa
  return $ VSet $ Sets.distUnion $ Sets.map coerceSet aa'

eval (BSDistIntersection aa) = do
  aa' <- evalAsSet aa
  return $ VSet $ Sets.distIntersect $ Sets.map coerceSet aa'

eval (BQSet s) = do
  s' <- evalAsSequence s
  return $ VSet $ Sets.fromList s'

eval (BSPowerset a) = do
  a' <- evalAsSet a
  return $ VSet $ Sets.map VSet $ Sets.powerset a'

eval (BSSequenceset a) = do
  a' <- evalAsSet a
  return $ VSet $ Sets.map VSequence $ Sets.sequenceset a'

-- Expressions that evaluate to a boolean

eval BBTrue  = return $ VBoolean True
eval BBFalse = return $ VBoolean False

eval (BBAnd b1 b2) = do
  b1' <- evalAsBoolean b1
  b2' <- evalAsBoolean b2
  return $ VBoolean $ b1' && b2'

eval (BBOr b1 b2) = do
  b1' <- evalAsBoolean b1
  b2' <- evalAsBoolean b2
  return $ VBoolean $ b1' || b2'

eval (BBNot b) = do
  b' <- evalAsBoolean b
  return $ VBoolean $ not b'

eval (BEqual x y) = do
  x' <- eval x
  y' <- eval y
  return $ VBoolean $ x' == y'

eval (BNotEqual x y) = do
  x' <- eval x
  y' <- eval y
  return $ VBoolean $ x' /= y'

eval (BLT x y) = do
  x' <- eval x
  y' <- eval y
  return $ VBoolean $ x' < y'

eval (BGT x y) = do
  x' <- eval x
  y' <- eval y
  return $ VBoolean $ x' > y'

eval (BLTE x y) = do
  x' <- eval x
  y' <- eval y
  return $ VBoolean $ x' <= y'

eval (BGTE x y) = do
  x' <- eval x
  y' <- eval y
  return $ VBoolean $ x' >= y'

eval (BQEmpty s) = do
  s' <- evalAsSequence s
  return $ VBoolean $ null s'

eval (BQIn x s) = do
  x' <- eval x
  s' <- evalAsSequence s
  return $ VBoolean $ x' `elem` s'

eval (BSIn x a) = do
  x' <- eval x
  a' <- evalAsSet a
  return $ VBoolean $ x' `Sets.member` a'

eval (BSEmpty a) = do
  a' <- evalAsSet a
  return $ VBoolean $ Sets.null a'

-- Expressions that evaluate to a tuple

eval (BTLit xs) = do
  xs' <- sequence $ map eval xs
  return $ VTuple xs'

-- Expressions that evaluate to a lambda

eval (BLambda e ids x) = return $ VLambda e ids x

-- Expressions that can evaluate to anything

eval (BQHead s) = do
  s' <- evalAsSequence s
  return $ head s'

eval (BIfThenElse b x y) = do
  b' <- evalAsBoolean b
  case b' of
    True  -> eval x
    False -> eval y

eval (BVar e id) = eval $ bind e $ lookupExpr e id

eval (BApply x ys) = do
  VLambda e0 ids body <- eval x
  let e1 = extendEnv e0 $ zipWith Binding ids (map EBound ys)
  eval $ bind e1 body
