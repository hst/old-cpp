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
import Test.QuickCheck

import HST.CSPM
import HST.CSPM.Tests.Generators

testAll = do
  putStr "SetLiteral: "
  quickCheck prop_SetLiteral
  putStr "SetClosedRange: "
  quickCheck prop_SetClosedRange
{-
  putStr "SetConcat: "
  quickCheck prop_SetConcat
  putStr "SetTail: "
  quickCheck prop_SetTail
-}

prop_SetLiteral = forAll (listOf enumber) tester
    where
      tester ns = s0 == s1
          where
            s0 = eval (ESet (SLit ns))
            s1 = VSet (nub (map eval ns))

prop_SetClosedRange = forAll (two enumber) tester
    where
      -- Don't test that i1 <= i2, because we want to ensure that we
      -- correctly get an empty set in that case.  Do ensure that the
      -- difference between the numbers isn't too large, so that we
      -- don't have to spend too much time verifying the sequence
      -- equality.
      tester (n1, n2) = (i2 - i1 <= 1000) ==> s0 == s1
          where
            s0 = eval (ESet (SClosedRange n1 n2))
            s1 = VSet (map VNumber [i1 .. i2])
            VNumber i1 = eval n1
            VNumber i2 = eval n2

{-
prop_SetConcat = forAll (two esequence) tester
    where
      tester (ns1, ns2) = v0 == v12
          where
            eq1 = ESet (QLit ns1)
            eq2 = ESet (QLit ns2)
            v0 = eval (ESet (QConcat eq1 eq2))
            v1 = eval eq1
            v2 = eval eq2
            VSet q1 = v1
            VSet q2 = v2
            v12 = VSet (q1 ++ q2)

prop_SetTail = forAll esequence tester
    where
      tester ns = (length ns > 0) ==> v0 == v1
          where
            eq0 = ESet (QTail eq1)
            eq1 = ESet (QLit ns)
            v0 = eval eq0
            v1' = eval eq1
            VSet q1' = v1'
            v1 = VSet (tail q1')
-}
