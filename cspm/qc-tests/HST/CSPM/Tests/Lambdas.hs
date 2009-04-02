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

module HST.CSPM.Tests.Lambdas where

import Test.QuickCheck hiding (evaluate)

import HST.CSPM
import HST.CSPM.Tests.Generators

testAll = do
  putStr "LambdaSum: "
  quickCheck prop_LambdaSum

prop_LambdaSum = forAll enumber tester
    where
      tester en = v0 == v1
          where
            f = Identifier "f"
            x = Identifier "x"
            e01 = ELambda [Clause
                           (PTuple [PIdentifier x])
                           (ENSum (EVar x) (ENLit 5))]
            e02 = EApply (EVar f) [en]
            e0 = ELet [DPatternDefn (PIdentifier f) e01] e02
            e1 = ENSum en (ENLit 5)
            v0 = evaluateRootExpression emptyScriptContext e0
            v1 = evaluateRootExpression emptyScriptContext e1
