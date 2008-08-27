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

module HST.CSPM.Tests.Booleans where

import Test.QuickCheck

import HST.CSPM
import HST.CSPM.Tests.Generators

testAll = do
  putStr "BoolAnd: "
  quickCheck prop_BoolAnd
  putStr "BoolOr: "
  quickCheck prop_BoolOr
  putStr "BoolNot: "
  quickCheck prop_BoolNot

prop_BoolAnd = forAll (two eboolean) tester
    where
      tester (eb1, eb2) = v0 == v12
          where
            v0 = eval (EBoolean (BAnd eb1 eb2))
            v1 = eval eb1
            v2 = eval eb2
            VBoolean b1 = v1
            VBoolean b2 = v2
            v12 = VBoolean (b1 && b2)

prop_BoolOr = forAll (two eboolean) tester
    where
      tester (eb1, eb2) = v0 == v12
          where
            v0 = eval (EBoolean (BOr eb1 eb2))
            v1 = eval eb1
            v2 = eval eb2
            VBoolean b1 = v1
            VBoolean b2 = v2
            v12 = VBoolean (b1 || b2)

prop_BoolNot = forAll eboolean tester
    where
      tester eb1 = v0 == v1
          where
            v0 = eval (EBoolean (BNot eb1))
            v1' = eval eb1
            VBoolean b1' = v1'
            v1 = VBoolean (not b1')
