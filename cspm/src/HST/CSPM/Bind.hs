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

module HST.CSPM.Bind (
                      bind
                     ) where

import Control.Monad.State

import HST.CSPM.Types
import HST.CSPM.Environments

data BindState
    = BindState ()

emptyState :: BindState
emptyState = BindState ()

type Binder a = State BindState a

bind :: Env -> Expression -> BoundExpression

bind e x = evalState (binder e x) emptyState

binder :: Env -> Expression -> Binder BoundExpression

binder1 f e x = do
  x' <- binder e x
  return $ f x'

binder2 f e x y = do
  x' <- binder e x
  y' <- binder e y
  return $ f x' y'

binder3 f e x y z = do
  x' <- binder e x
  y' <- binder e y
  z' <- binder e z
  return $ f x' y' z'

binderList f e xs = do
  xs' <- sequence $ map (binder e) xs
  return $ f xs'

binder e EBottom = return BBottom

-- Numbers

binder e (ENLit i)         = return $ BNLit i
binder e (ENNeg x)         = binder1 BNNeg e x
binder e (ENSum x y)       = binder2 BNSum e x y
binder e (ENDiff x y)      = binder2 BNDiff e x y
binder e (ENProd x y)      = binder2 BNProd e x y
binder e (ENQuot x y)      = binder2 BNQuot e x y
binder e (ENRem x y)       = binder2 BNRem e x y
binder e (EQLength x)      = binder1 BQLength e x
binder e (ESCardinality x) = binder1 BSCardinality e x

-- Sequences

binder e (EQLit xs)          = binderList BQLit e xs
binder e (EQClosedRange x y) = binder2 BQClosedRange e x y
binder e (EQOpenRange x)     = binder1 BQOpenRange e x
binder e (EQConcat x y)      = binder2 BQConcat e x y
binder e (EQTail x)          = binder1 BQTail e x

-- Sets

binder e (ESLit xs)             = binderList BSLit e xs
binder e (ESClosedRange x y)    = binder2 BSClosedRange e x y
binder e (ESOpenRange x)        = binder1 BSOpenRange e x
binder e (ESUnion x y)          = binder2 BSUnion e x y
binder e (ESIntersection x y)   = binder2 BSIntersection e x y
binder e (ESDifference x y)     = binder2 BSDifference e x y
binder e (ESDistUnion x)        = binder1 BSDistUnion e x
binder e (ESDistIntersection x) = binder1 BSDistIntersection e x
binder e (EQSet x)              = binder1 BQSet e x
binder e (ESPowerset x)         = binder1 BSPowerset e x
binder e (ESSequenceset x)      = binder1 BSSequenceset e x

-- Booleans

binder e EBTrue          = return BBTrue
binder e EBFalse         = return BBFalse
binder e (EBAnd x y)     = binder2 BBAnd e x y
binder e (EBOr x y)      = binder2 BBOr e x y
binder e (EBNot x)       = binder1 BBNot e x
binder e (EEqual x y)    = binder2 BEqual e x y
binder e (ENotEqual x y) = binder2 BNotEqual e x y
binder e (ELT x y)       = binder2 BLT e x y
binder e (EGT x y)       = binder2 BGT e x y
binder e (ELTE x y)      = binder2 BLTE e x y
binder e (EGTE x y)      = binder2 BGTE e x y
binder e (EQEmpty x)     = binder1 BQEmpty e x
binder e (EQIn x y)      = binder2 BQIn e x y
binder e (ESIn x y)      = binder2 BSIn e x y
binder e (ESEmpty x)     = binder1 BSEmpty e x

-- Tuples

binder e (ETLit xs) = binderList BTLit e xs

-- Lambdas

binder e (ELambda ids x) = return $ BLambda e ids x

-- Anything

binder e (EQHead x)          = binder1 BQHead e x
binder e (EIfThenElse x y z) = binder3 BIfThenElse e x y z

binder e (EBound be) = return $ be

binder e (EVar id) = return $ BVar e id

binder e (ELet bs x) = binder e1 x
    where
      e1 = extendEnv e bs

binder e (EApply x ys) = do
  x' <- binder e x
  ys' <- sequence $ map (binder e) ys
  return $ BApply x' ys'
