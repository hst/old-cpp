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

module HST.CSPM.Tests.Patterns where

import Test.QuickCheck hiding (evaluate)

import HST.CSPM
import HST.CSPM.Tests.Generators

import qualified HST.CSPM.Sets as Sets

testAll = do
  putStr "PatternInt: "
  quickCheck prop_PatternInt
  putStr "PatternWildcard: "
  quickCheck prop_PatternWildcard
  putStr "PatternIdentifier: "
  quickCheck prop_PatternIdentifier
  putStr "PatternWildcardTuple: "
  quickCheck prop_PatternWildcardTuple
  putStr "PatternWildcardTupleTooLong: "
  quickCheck prop_PatternWildcardTupleTooLong
  putStr "PatternWildcardTupleTooShort: "
  quickCheck prop_PatternWildcardTupleTooShort
  putStr "PatternIdentifierTuple: "
  quickCheck prop_PatternIdentifierTuple
  putStr "PatternWildcardSequence: "
  quickCheck prop_PatternWildcardSequence
  putStr "PatternWildcardSequenceTooLong: "
  quickCheck prop_PatternWildcardSequenceTooLong
  putStr "PatternWildcardSequenceTooShort: "
  quickCheck prop_PatternWildcardSequenceTooShort
  putStr "PatternIdentifierSequence: "
  quickCheck prop_PatternIdentifierSequence
  putStr "PatternConcat1: "
  quickCheck prop_PatternConcat1
  putStr "PatternConcat2: "
  quickCheck prop_PatternConcat2
  putStr "PatternEmptySet: "
  quickCheck prop_PatternEmptySet
  putStr "PatternSingletonSet: "
  quickCheck prop_PatternSingletonSet
  putStr "PatternConjunctionInt: "
  quickCheck prop_PatternConjunctionInt

prop_PatternInt i0 = valueMatches p0 v0 == Just []
    where
      p0 = PNLit i0
      v0 = VNumber i0
      types = i0 :: Int

prop_PatternWildcard = forAll expression tester
    where
      tester x = valueMatches PWildcard v == Just []
          where
            v = evaluate emptyRootEnv x

prop_PatternIdentifier = forAll expression tester
    where
      tester x = valueMatches (PIdentifier id) v == Just [b]
          where
            id = Identifier "x"
            v  = evaluate emptyRootEnv x
            b  = Binding id (EValue v)

prop_PatternWildcardTuple = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p vs == Just []
          where
            p  = PTuple [PWildcard, PWildcard]
            vs = VTuple [evaluate emptyRootEnv x1,
                         evaluate emptyRootEnv x2]

prop_PatternWildcardTupleTooLong = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p vs == Nothing
          where
            p  = PTuple [PWildcard, PWildcard, PWildcard]
            vs = VTuple [evaluate emptyRootEnv x1,
                         evaluate emptyRootEnv x2]

prop_PatternWildcardTupleTooShort = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p vs == Nothing
          where
            p  = PTuple [PWildcard]
            vs = VTuple [evaluate emptyRootEnv x1,
                         evaluate emptyRootEnv x2]

prop_PatternIdentifierTuple = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p v == Just [b1, b2]
          where
            p   = PTuple [PIdentifier id1, PIdentifier id2]
            id1 = Identifier "x"
            id2 = Identifier "y"
            v   = VTuple [v1, v2]
            v1  = evaluate emptyRootEnv x1
            v2  = evaluate emptyRootEnv x2
            b1  = Binding id1 (EValue v1)
            b2  = Binding id2 (EValue v2)

prop_PatternWildcardSequence = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p vs == Just []
          where
            p  = PQLit [PWildcard, PWildcard]
            vs = VSequence [evaluate emptyRootEnv x1,
                            evaluate emptyRootEnv x2]

prop_PatternWildcardSequenceTooLong = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p vs == Nothing
          where
            p  = PQLit [PWildcard, PWildcard, PWildcard]
            vs = VSequence [evaluate emptyRootEnv x1,
                            evaluate emptyRootEnv x2]

prop_PatternWildcardSequenceTooShort = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p vs == Nothing
          where
            p  = PQLit [PWildcard]
            vs = VSequence [evaluate emptyRootEnv x1,
                            evaluate emptyRootEnv x2]

prop_PatternIdentifierSequence = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p v == Just [b1, b2]
          where
            p   = PQLit [PIdentifier id1, PIdentifier id2]
            id1 = Identifier "x"
            id2 = Identifier "y"
            v   = VSequence [v1, v2]
            v1  = evaluate emptyRootEnv x1
            v2  = evaluate emptyRootEnv x2
            b1  = Binding id1 (EValue v1)
            b2  = Binding id2 (EValue v2)

prop_PatternConcat1 = forAll (two expression) tester
    where
      tester (x1, x2) = valueMatches p v == Just [b1, b2]
          where
            p   = PQConcat (PQLit [PIdentifier id1]) (PIdentifier id2)
            id1 = Identifier "x"
            id2 = Identifier "y"
            v   = VSequence [v1, v2]
            v1  = evaluate emptyRootEnv x1
            v2  = evaluate emptyRootEnv x2
            b1  = Binding id1 (EValue v1)
            b2  = Binding id2 (EValue (VSequence [v2]))

prop_PatternConcat2 = forAll (three expression) tester
    where
      tester (x1, x2, x3) = valueMatches p v == Just [b1, b2, b3]
          where
            p1  = PQConcat (PQLit [PIdentifier id1]) (PIdentifier id2)
            p   = PQConcat p1 (PQLit [PIdentifier id3])
            id1 = Identifier "x"
            id2 = Identifier "y"
            id3 = Identifier "z"
            v   = VSequence [v1, v2, v3]
            v1  = evaluate emptyRootEnv x1
            v2  = evaluate emptyRootEnv x2
            v3  = evaluate emptyRootEnv x3
            b1  = Binding id1 (EValue v1)
            b2  = Binding id2 (EValue (VSequence [v2]))
            b3  = Binding id3 (EValue v3)

prop_PatternEmptySet = forAll eset tester
    where
      tester a = valueMatches PSEmpty v == result
          where
            v = evaluate emptyRootEnv a
            VSet vset = v
            result = case Sets.null vset of
                       True  -> Just []
                       False -> Nothing

prop_PatternSingletonSet = forAll eset tester
    where
      tester a = valueMatches (PSSingleton PWildcard) v == result
          where
            v = evaluate emptyRootEnv a
            VSet vset = v
            result = case Sets.toList vset of
                       [_] -> Just []
                       _   -> Nothing

prop_PatternConjunctionInt i0 = valueMatches p0 v0 == Just [b]
    where
      id = Identifier "x"
      p0 = PConjunction (PIdentifier id) (PNLit i0)
      v0 = VNumber i0
      b  = Binding id (EValue v0)
      types = i0 :: Int
