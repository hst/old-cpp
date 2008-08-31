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

module HST.CSPM.Bind where

import HST.CSPM.Types
import HST.CSPM.Environments

bind :: Env -> Expression -> BoundExpression

bind e EBottom = BBottom

-- Numbers

bind e (ENLit i)         = BNLit i
bind e (ENNeg x)         = BNNeg (bind e x)
bind e (ENSum x y)       = BNSum (bind e x) (bind e y)
bind e (ENDiff x y)      = BNDiff (bind e x) (bind e y)
bind e (ENProd x y)      = BNProd (bind e x) (bind e y)
bind e (ENQuot x y)      = BNQuot (bind e x) (bind e y)
bind e (ENRem x y)       = BNRem (bind e x) (bind e y)
bind e (EQLength x)      = BQLength (bind e x)
bind e (ESCardinality x) = BSCardinality (bind e x)

-- Sequences

bind e (EQLit xs)          = BQLit (map (bind e) xs)
bind e (EQClosedRange x y) = BQClosedRange (bind e x) (bind e y)
bind e (EQOpenRange x)     = BQOpenRange (bind e x)
bind e (EQConcat x y)      = BQConcat (bind e x) (bind e y)
bind e (EQTail x)          = BQTail (bind e x)

-- Sets

bind e (ESLit xs)             = BSLit (map (bind e) xs)
bind e (ESClosedRange x y)    = BSClosedRange (bind e x) (bind e y)
bind e (ESOpenRange x)        = BSOpenRange (bind e x)
bind e (ESUnion x y)          = BSUnion (bind e x) (bind e y)
bind e (ESIntersection x y)   = BSIntersection (bind e x) (bind e y)
bind e (ESDifference x y)     = BSDifference (bind e x) (bind e y)
bind e (ESDistUnion x)        = BSDistUnion (bind e x)
bind e (ESDistIntersection x) = BSDistIntersection (bind e x)
bind e (EQSet x)              = BQSet (bind e x)
bind e (ESPowerset x)         = BSPowerset (bind e x)
bind e (ESSequenceset x)      = BSSequenceset (bind e x)

-- Booleans

bind e EBTrue          = BBTrue
bind e EBFalse         = BBFalse
bind e (EBAnd x y)     = BBAnd (bind e x) (bind e y)
bind e (EBOr x y)      = BBOr (bind e x) (bind e y)
bind e (EBNot x)       = BBNot (bind e x)
bind e (EEqual x y)    = BEqual (bind e x) (bind e y)
bind e (ENotEqual x y) = BNotEqual (bind e x) (bind e y)
bind e (ELT x y)       = BLT (bind e x) (bind e y)
bind e (EGT x y)       = BGT (bind e x) (bind e y)
bind e (ELTE x y)      = BLTE (bind e x) (bind e y)
bind e (EGTE x y)      = BGTE (bind e x) (bind e y)
bind e (EQEmpty x)     = BQEmpty (bind e x)
bind e (EQIn x y)      = BQIn (bind e x) (bind e y)
bind e (ESIn x y)      = BSIn (bind e x) (bind e y)
bind e (ESEmpty x)     = BSEmpty (bind e x)

-- Tuples

bind e (ETLit xs) = BTLit (map (bind e) xs)

-- Lambdas

bind e (ELambda ids x) = BLambda e ids x

-- Anything

bind e (EQHead x)          = BQHead (bind e x)
bind e (EIfThenElse x y z) = BIfThenElse (bind e x) (bind e y) (bind e z)

bind e (EBound be) = be

bind e (EVar id) = BVar e id

bind e (ELet bs x) = bind e1 x
    where
      e1 = extendEnv e bs

bind e (EApply x ys) = BApply (bind e x) (map (bind e) ys)
