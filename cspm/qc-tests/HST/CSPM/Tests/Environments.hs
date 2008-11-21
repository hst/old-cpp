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

module HST.CSPM.Tests.Environments where

import Test.QuickCheck

import HST.CSPM
import HST.CSPM.Tests.Generators

testAll = do
  putStr "EnvExtend1: "
  quickCheck prop_EnvExtend1

prop_EnvExtend1 = forAll (pair identifier expression) tester
    where
      tester (id, x) = x == x0
          where
            e0 = extendEnv rootEnv [Binding id x]
            x0 = lookupExpr e0 id
