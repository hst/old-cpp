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

module HST.CSPM.Tests.Sets where

import Data.List
import Test.QuickCheck hiding (evaluate)

import HST.CSPM
import qualified HST.CSPM.Sets as Sets
import HST.CSPM.Tests.Generators

testAll = do
  putStr "SetLiteral: "
  quickCheck prop_SetLiteral
  putStr "SetClosedRange: "
  quickCheck prop_SetClosedRange
  {-
  putStr "SetOpenRange: "
  quickCheck prop_SetOpenRange
  -}
  putStr "SetUnionAssoc: "
  quickCheck prop_SetUnionAssoc
  putStr "SetIntersectAssoc: "
  quickCheck prop_SetIntersectAssoc

prop_SetLiteral = forAll (listOf enumber) tester
    where
      tester ns = s0 == s1
          where
            s0 = evaluateRootExpression emptyScriptContext (ESLit ns)
            s1 = VSet $ Sets.fromList $
                 (map (evaluateRootExpression emptyScriptContext) ns)

prop_SetClosedRange = forAll (two enumber) tester
    where
      -- Don't test that i1 <= i2, because we want to ensure that we
      -- correctly get an empty set in that case.  Do ensure that the
      -- difference between the numbers isn't too large, so that we
      -- don't have to spend too much time verifying the sequence
      -- equality.
      tester (n1, n2) = (i2 - i1 <= 1000) ==> s0 == s1
          where
            s0 = evaluateRootExpression emptyScriptContext (ESClosedRange n1 n2)
            s1 = VSet $ Sets.fromList (map VNumber [i1 .. i2])
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

{-
Can't test for equality on infinite sets.

prop_SetOpenRange = forAll enumber tester
    where
      tester n = s0 == s1
          where
            s0 = evaluateRootExpression emptyScriptContext (ESOpenRange n)
            s1 = VSet $ Sets.fromList $ map VNumber [i..]
            i  = evaluateWith evalAsNumber emptyScriptContext n
-}

prop_SetUnionAssoc = forAll (two eset) tester
    where
      tester (es1, es2) = s1 == s2
          where
            s1 = evaluateRootExpression emptyScriptContext (ESUnion es1 es2)
            s2 = evaluateRootExpression emptyScriptContext (ESUnion es2 es1)

prop_SetIntersectAssoc = forAll (two eset) tester
    where
      tester (es1, es2) = s1 == s2
          where
            s1 = evaluateRootExpression emptyScriptContext (ESIntersection es1 es2)
            s2 = evaluateRootExpression emptyScriptContext (ESIntersection es2 es1)
