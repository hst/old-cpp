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
import qualified Data.Set as DS

import HST.CSP0 hiding (processes)
import HST.CSPM.Types
import HST.CSPM.Environments

data BindState
    = BindState {
        processes :: ProcessSet
      }

instance HasProcessSet BindState where
    getProcessSet = processes
    putProcessSet s ps = s { processes = ps }

emptyState :: BindState
emptyState = BindState $ ProcessSet DS.empty

type Binder a = State BindState a

bind :: String -> Env -> Expression -> BoundExpression

bind pfx e x = evalState (binder pfx e x) emptyState

binder :: String -> Env -> Expression -> Binder BoundExpression

binder1 pfx f e x = do
  x' <- binder pfx e x
  return $ f x'

binder2 pfx f e x y = do
  x' <- binder pfx e x
  y' <- binder pfx e y
  return $ f x' y'

binder3 pfx f e x y z = do
  x' <- binder pfx e x
  y' <- binder pfx e y
  z' <- binder pfx e z
  return $ f x' y' z'

binder4 pfx f e w x y z = do
  w' <- binder pfx e w
  x' <- binder pfx e x
  y' <- binder pfx e y
  z' <- binder pfx e z
  return $ f w' x' y' z'

binderList pfx f e xs = do
  xs' <- sequence $ map (binder pfx e) xs
  return $ f xs'

binder pfx e EBottom = return BBottom

-- Numbers

binder pfx e (ENLit i)         = return $ BNLit i
binder pfx e (ENNeg x)         = binder1 pfx BNNeg e x
binder pfx e (ENSum x y)       = binder2 pfx BNSum e x y
binder pfx e (ENDiff x y)      = binder2 pfx BNDiff e x y
binder pfx e (ENProd x y)      = binder2 pfx BNProd e x y
binder pfx e (ENQuot x y)      = binder2 pfx BNQuot e x y
binder pfx e (ENRem x y)       = binder2 pfx BNRem e x y
binder pfx e (EQLength x)      = binder1 pfx BQLength e x
binder pfx e (ESCardinality x) = binder1 pfx BSCardinality e x

-- Sequences

binder pfx e (EQLit xs)          = binderList pfx BQLit e xs
binder pfx e (EQClosedRange x y) = binder2 pfx BQClosedRange e x y
binder pfx e (EQOpenRange x)     = binder1 pfx BQOpenRange e x
binder pfx e (EQConcat x y)      = binder2 pfx BQConcat e x y
binder pfx e (EQTail x)          = binder1 pfx BQTail e x

-- Sets

binder pfx e (ESLit xs)             = binderList pfx BSLit e xs
binder pfx e (ESClosedRange x y)    = binder2 pfx BSClosedRange e x y
binder pfx e (ESOpenRange x)        = binder1 pfx BSOpenRange e x
binder pfx e (ESUnion x y)          = binder2 pfx BSUnion e x y
binder pfx e (ESIntersection x y)   = binder2 pfx BSIntersection e x y
binder pfx e (ESDifference x y)     = binder2 pfx BSDifference e x y
binder pfx e (ESDistUnion x)        = binder1 pfx BSDistUnion e x
binder pfx e (ESDistIntersection x) = binder1 pfx BSDistIntersection e x
binder pfx e (EQSet x)              = binder1 pfx BQSet e x
binder pfx e (ESPowerset x)         = binder1 pfx BSPowerset e x
binder pfx e (ESSequenceset x)      = binder1 pfx BSSequenceset e x

-- Booleans

binder pfx e EBTrue          = return BBTrue
binder pfx e EBFalse         = return BBFalse
binder pfx e (EBAnd x y)     = binder2 pfx BBAnd e x y
binder pfx e (EBOr x y)      = binder2 pfx BBOr e x y
binder pfx e (EBNot x)       = binder1 pfx BBNot e x
binder pfx e (EEqual x y)    = binder2 pfx BEqual e x y
binder pfx e (ENotEqual x y) = binder2 pfx BNotEqual e x y
binder pfx e (ELT x y)       = binder2 pfx BLT e x y
binder pfx e (EGT x y)       = binder2 pfx BGT e x y
binder pfx e (ELTE x y)      = binder2 pfx BLTE e x y
binder pfx e (EGTE x y)      = binder2 pfx BGTE e x y
binder pfx e (EQEmpty x)     = binder1 pfx BQEmpty e x
binder pfx e (EQIn x y)      = binder2 pfx BQIn e x y
binder pfx e (ESIn x y)      = binder2 pfx BSIn e x y
binder pfx e (ESEmpty x)     = binder1 pfx BSEmpty e x

-- Tuples

binder pfx e (ETLit xs) = binderList pfx BTLit e xs

-- Lambdas

binder pfx e (ELambda ids x) = return $ BLambda e ids x

-- Anything

binder pfx e (EQHead x)          = binder1 pfx BQHead e x
binder pfx e (EIfThenElse x y z) = binder3 pfx BIfThenElse e x y z

binder pfx e (EBound be) = return $ be

binder pfx e (EVar id) = return $ BVar e id

binder pfx e (ELet bs x) = binder pfx e1 x
    where
      e1   = extendEnv pfx1 e bs
      pfx1 = pfx ++ "."

binder pfx e (EApply x ys) = do
  x' <- binder pfx e x
  ys' <- sequence $ map (binder pfx e) ys
  return $ BApply x' ys'

-- Events

binder pfx e (EELit a) = return $ BELit a

-- Processes

binder pfx e (EPrefix a p) = do
  dest <- newProcess pfx
  binder2 pfx (BPrefix dest) e a p

binder pfx e (EExtChoice p q) = do
  dest <- newProcess pfx
  binder2 pfx (BExtChoice dest) e p q

binder pfx e (EIntChoice p q) = do
  dest <- newProcess pfx
  binder2 pfx (BIntChoice dest) e p q

binder pfx e (ETimeout p q) = do
  dest <- newProcess pfx
  binder2 pfx (BTimeout dest) e p q

binder pfx e (ESeqComp p q) = do
  dest <- newProcess pfx
  binder2 pfx (BSeqComp dest) e p q

binder pfx e (EInterleave p q) = do
  dest <- newProcess pfx
  binder2 pfx (BInterleave dest) e p q

binder pfx e (EIParallel p alpha q) = do
  dest <- newProcess pfx
  binder3 pfx (BIParallel dest) e p alpha q

binder pfx e (EAParallel p alpha beta q) = do
  dest <- newProcess pfx
  binder4 pfx (BAParallel dest) e p alpha beta q

binder pfx e (EHide p alpha) = do
  dest <- newProcess pfx
  binder2 pfx (BHide dest) e p alpha

binder pfx e (ERExtChoice ps) = do
  dest <- newProcess pfx
  binder1 pfx (BRExtChoice dest) e ps
