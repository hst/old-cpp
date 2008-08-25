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

module HST.CSPM.Tests.Generators where

import Monad
import Test.QuickCheck

import HST.CSPM

checkAll checker props =
    foldl (>>) (return ()) $ map checker props

number :: Gen Number
number = sized number'
    where
      number' 0         = liftM NLit arbitrary
      number' n | n > 0 = oneof [liftM NLit arbitrary,
                                 liftM NNeg subnum,
                                 liftM2 NSum subnum subnum,
                                 liftM2 NDiff subnum subnum,
                                 liftM2 NProd subnum subnum
                                 --liftM2 NQuot subnum subnum,
                                 --liftM2 NRem subnum subnum
                                ]
                where
                  subnum = liftM ENumber $ number' (n `div` 2)

enumber :: Gen Expression
enumber = liftM ENumber number

listOf :: Gen a -> Gen [a]
listOf g = sized listOf'
    where
      listOf' n = sequence [g | i <- [1..n]]

esequence = listOf enumber
