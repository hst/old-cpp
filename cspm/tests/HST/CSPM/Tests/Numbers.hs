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

import Test.QuickCheck

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
      v1 = eval (ENumber (NLit i))
      v2 = VNumber i
      types = i :: Int

prop_NumNeg = forAll enumber tester
    where
      tester n = i0 == negate i1
          where
            VNumber i0 = eval (ENumber (NNeg n))
            VNumber i1 = eval n

prop_NumSum = forAll (two enumber) tester
    where
      tester (n1, n2) = i0 == i1 + i2
          where
            VNumber i0 = eval (ENumber (NSum n1 n2))
            VNumber i1 = eval n1
            VNumber i2 = eval n2

prop_NumDiff = forAll (two enumber) tester
    where
      tester (n1, n2) = i0 == i1 - i2
          where
            VNumber i0 = eval (ENumber (NDiff n1 n2))
            VNumber i1 = eval n1
            VNumber i2 = eval n2

prop_NumProd = forAll (two enumber) tester
    where
      tester (n1, n2) = i0 == i1 * i2
          where
            VNumber i0 = eval (ENumber (NProd n1 n2))
            VNumber i1 = eval n1
            VNumber i2 = eval n2

prop_NumLT = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 < i2)
          where
            v0 = eval (EBoolean (ELT n1 n2))
            VBoolean b0 = v0
            VNumber i1 = eval n1
            VNumber i2 = eval n2

prop_NumGT = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 > i2)
          where
            v0 = eval (EBoolean (EGT n1 n2))
            VBoolean b0 = v0
            VNumber i1 = eval n1
            VNumber i2 = eval n2

prop_NumLTE = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 <= i2)
          where
            v0 = eval (EBoolean (ELTE n1 n2))
            VBoolean b0 = v0
            VNumber i1 = eval n1
            VNumber i2 = eval n2

prop_NumGTE = forAll (two enumber) tester
    where
      tester (n1, n2) = b0 == (i1 >= i2)
          where
            v0 = eval (EBoolean (EGTE n1 n2))
            VBoolean b0 = v0
            VNumber i1 = eval n1
            VNumber i2 = eval n2
