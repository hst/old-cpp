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

module HST.CSPM.Tests.Sequences where

import Test.QuickCheck hiding (evaluate)

import HST.CSPM
import HST.CSPM.Tests.Generators

testAll = do
  putStr "SeqLiteral: "
  quickCheck prop_SeqLiteral
  putStr "SeqClosedRange: "
  quickCheck prop_SeqClosedRange
  putStr "SeqConcat: "
  quickCheck prop_SeqConcat
  putStr "SeqTail: "
  quickCheck prop_SeqTail

prop_SeqLiteral = forAll (listOf enumber) tester
    where
      tester ns = q0 == q1
          where
            q0 = evaluateRootExpression emptyScriptContext (EQLit ns)
            q1 = VSequence (map (evaluateRootExpression emptyScriptContext) ns)

prop_SeqClosedRange = forAll (two enumber) tester
    where
      -- Don't test that i1 <= i2, because we want to ensure that we
      -- correctly get an empty sequence in that case.  Do ensure that
      -- the difference between the numbers isn't too large, so that
      -- we don't have to spend too much time verifying the sequence
      -- equality.
      tester (n1, n2) = (i2 - i1 <= 1000) ==> q0 == q1
          where
            q0 = evaluateRootExpression emptyScriptContext (EQClosedRange n1 n2)
            q1 = VSequence (map VNumber [i1 .. i2])
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_SeqConcat = forAll (two esequence) tester
    where
      tester (eq1, eq2) = v0 == v12
          where
            v0 = evaluateRootExpression emptyScriptContext (EQConcat eq1 eq2)
            q1 = evaluateWith evalAsSequence emptyScriptContext eq1
            q2 = evaluateWith evalAsSequence emptyScriptContext eq2
            v12 = VSequence (q1 ++ q2)

prop_SeqTail = forAll esequence tester
    where
      tester eq = (length ns > 0) ==> v0 == v1
          where
            eq0 = EQTail eq
            v0  = evaluateRootExpression emptyScriptContext eq0

            EQLit ns = eq
            eq1      = EQLit (tail ns)
            v1       = evaluateRootExpression emptyScriptContext eq1
