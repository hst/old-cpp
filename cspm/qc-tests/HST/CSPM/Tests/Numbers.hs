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

module HST.CSPM.Tests.Numbers where

import Test.QuickCheck hiding (evaluate)

import HST.CSPM
import HST.CSPM.Tests.Generators

testAll = do
  putStr "NumLiteral: "
  quickCheck prop_NumLiteral
  putStr "NumNeg: "
  quickCheck prop_NumNeg
  putStr "NumSum: "
  quickCheck prop_NumSum
  putStr "NumDiff: "
  quickCheck prop_NumDiff
  putStr "NumProd: "
  quickCheck prop_NumProd
  putStr "NumLT: "
  quickCheck prop_NumLT
  putStr "NumGT: "
  quickCheck prop_NumGT
  putStr "NumLTE: "
  quickCheck prop_NumLTE
  putStr "NumGTE: "
  quickCheck prop_NumGTE

prop_NumLiteral i = v1 == v2
    where
      v1 = evaluateRootExpression emptyScriptContext (ENLit i)
      v2 = VNumber i
      types = i :: Int

prop_NumNeg = forAll enumber tester
    where
      tester n = i0 == negate i1
          where
            i0 = evaluateWith evalAsNumber emptyScriptContext (ENNeg n)
            i1 = evaluateWith evalAsNumber emptyScriptContext n

prop_NumSum = forAll (two enumber) tester
    where
      tester (n1, n2) = i0 == i1 + i2
          where
            i0 = evaluateWith evalAsNumber emptyScriptContext (ENSum n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_NumDiff = forAll (two enumber) tester
    where
      tester (n1, n2) = i0 == i1 - i2
          where
            i0 = evaluateWith evalAsNumber emptyScriptContext (ENDiff n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_NumProd = forAll (two enumber) tester
    where
      tester (n1, n2) = i0 == i1 * i2
          where
            i0 = evaluateWith evalAsNumber emptyScriptContext (ENProd n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_NumLT = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 < i2)
          where
            b0 = evaluateWith evalAsBoolean emptyScriptContext (ELT n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_NumGT = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 > i2)
          where
            b0 = evaluateWith evalAsBoolean emptyScriptContext (EGT n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_NumLTE = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 <= i2)
          where
            b0 = evaluateWith evalAsBoolean emptyScriptContext (ELTE n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2

prop_NumGTE = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 >= i2)
          where
            b0 = evaluateWith evalAsBoolean emptyScriptContext (EGTE n1 n2)
            i1 = evaluateWith evalAsNumber emptyScriptContext n1
            i2 = evaluateWith evalAsNumber emptyScriptContext n2
