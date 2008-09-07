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


listOf :: Gen a -> Gen [a]
listOf g = sized listOf'
    where
      listOf' n = sequence [g | i <- [1..n]]

pair :: Gen a -> Gen b -> Gen (a, b)
pair = liftM2 (curry id)


enumber :: Gen Expression
enumber = sized number'
    where
      number' 0         = liftM ENLit arbitrary
      number' n | n > 0 = oneof [liftM  ENLit arbitrary,
                                 liftM  ENNeg subnum,
                                 liftM2 ENSum subnum subnum,
                                 liftM2 ENDiff subnum subnum,
                                 liftM2 ENProd subnum subnum
                                 --liftM2 ENQuot subnum subnum,
                                 --liftM2 ENRem subnum subnum
                                ]
                where
                  subnum = number' (n `div` 2)


esequence = liftM EQLit $ listOf enumber

eset = liftM ESLit $ listOf enumber

eboolean :: Gen Expression
eboolean = sized boolean'
    where
      boolean' 0         = oneof [return EBTrue, return EBFalse]
      boolean' n | n > 0 = oneof [liftM2 EBAnd subbool subbool,
                                  liftM2 EBOr subbool subbool,
                                  liftM  EBNot subbool
                                 ]
                 where
                   subbool = boolean' (n `div` 2)

expression = oneof [enumber, esequence, eboolean]

identifier = elements $ map (Identifier . (:[])) ['a'..'z']
