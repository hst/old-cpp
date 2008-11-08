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

module Main where

import qualified HST.CSPM.Tests.Booleans as Booleans
import qualified HST.CSPM.Tests.Numbers as Numbers
import qualified HST.CSPM.Tests.Sequences as Sequences
import qualified HST.CSPM.Tests.Sets as Sets
import qualified HST.CSPM.Tests.Lets as Lets
import qualified HST.CSPM.Tests.Lambdas as Lambdas

main = do
  Booleans.testAll
  Numbers.testAll
  Sequences.testAll
  Sets.testAll
  Lets.testAll
  Lambdas.testAll
