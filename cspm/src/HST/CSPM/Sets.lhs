-----------------------------------------------------------------------
  Copyright © 2008 Douglas Creager

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later
    version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
    MA 02111-1307 USA
------------------------------------------------------------------------

> module HST.CSPM.Sets where

> import Prelude hiding (null)

> import Data.Set (Set)
> import qualified Data.Set as Set

> import HST.CSPM.Types

The empty ranged set.

> emptyRangedSet :: RangedSet
> emptyRangedSet = RangedSet { elems = Set.empty, from = Nothing }

Given a list of Values, we can create a RangedSet by creating an elems
set from this list.

> fromList :: [Value] -> RangedSet
> fromList xs = RangedSet {
>                 elems = Set.fromList xs,
>                 from  = Nothing
>               }

Create a RangedSet that only contains the given open range of Ints.

> openRange :: Int -> RangedSet
> openRange i = RangedSet {
>                 elems = Set.empty,
>                 from  = Just i
>               }


> member :: Value -> RangedSet -> Bool
> member v@(VNumber i) (RangedSet e (Just f)) = v `Set.member` e ||
>                                               i >= f
> member v rs = v `Set.member` (elems rs)

> null :: RangedSet -> Bool
> null (RangedSet _ (Just _)) = False
> null (RangedSet e Nothing)  = Set.null e

> size :: RangedSet -> Value
> size (RangedSet e Nothing)  = VNumber $ Set.size e
> size (RangedSet _ (Just _)) = VBottom

> toList :: RangedSet -> [Value]
> toList (RangedSet e Nothing)  = Set.toList e
> toList (RangedSet e (Just i)) = Set.toList e ++ map VNumber [i..]

Consolidating RangedSets

We'd like to ensure that any Int elements that are covered by the from
clause aren't also included in the elems clause.  This ensures that we
have a canonical representation for RangedSets.

To do this, we first define the foldNumbers function, which takes a
list of Ints and a starting from value, and filters out any elements
at the end of the list that can be merged into the from clause.  It
returns the (possibly modified) from value, and a Set of the remaining
Ints.

> foldNumbers :: Int -> [Int] -> (Set Int, Int)
> foldNumbers from = foldr folder (Set.empty, from)
>     where
>       folder i (nums, f) | i >= f     = (nums, f)
>                          | i == (f-1) = (nums, i)
>                          | otherwise  = (Set.insert i nums, f)

Now, we can use foldNumbers to define consolidate.  If there is no
from clause, then we don't have to do anything; the elems clause is
already canonical.

> consolidate :: RangedSet -> RangedSet
> consolidate rs@(RangedSet { from = Nothing }) = rs

Otherwise, we first separate the explicit elements into numbers
(numElems) and non-numbers (otherElems).  We then use foldNumbers to
merge numElems into the from clause, giving as a new Set of Ints
(ints), and a new from claus (from').  We need to apply VNumber to
each of the Ints to turn them into proper Values.

At this point, we have a new Set of VNumber elems (numElems'), the
original Set of non-number Values (otherElems), and the new from
clause (from'), which we can combine together into the consolidated
RangedSet.

> consolidate rs = case from rs of
>                    Nothing -> rs
>                    Just i  -> RangedSet {
>                                 elems = numElems' `Set.union` otherElems,
>                                 from  = Just from'
>                               }
>                        where
>                          (ints, from') = foldNumbers i numList
>                          numElems'     = Set.map VNumber ints
>    where
>      (numElems, otherElems) = Set.partition keepNumbers (elems rs)
>      keepNumbers (VNumber _) = True
>      keepNumbers _           = False
>
>      numList = map toInt $ Set.toAscList numElems
>      toInt (VNumber i) = i


Unions of RangedSets

  x = (xE ∪ xF)
  y = (yE ∪ yF)

To union two RangedSets, we can union the elem and from clauses
independently:

  x ∪ y = (xE ∪ xF) ∪ (yE ∪ yF)
        = (xE ∪ yE) ∪ (xF ∪ yF)                   [assoc.]

For the from clauses, we have different cases depending on whether
either of the operands are empty or not:

    ∅   ∪   ∅   = ∅
  {i..} ∪   ∅   = {i..}
    ∅   ∪ {j..} = {j..}
  {i..} ∪ {j..} = {min(i,j)..}

> union :: RangedSet -> RangedSet -> RangedSet
> union x y = consolidate $ RangedSet {
>               elems = xE_yE,
>               from  = xF_yF (from x) (from y)
>             }
>     where
>       xE_yE = (elems x) `Set.union` (elems y)
>
>       xF_yF Nothing Nothing   = Nothing
>       xF_yF (Just i) Nothing  = Just i
>       xF_yF Nothing (Just j)  = Just j
>       xF_yF (Just i) (Just j) = Just (min i j)


Intersection of RangedSets

  x = (xE ∪ xF)
  y = (yE ∪ yF)

To intersect two RangedSets, we can distribute the intersection into
the unions:

  x ∩ y = (xE ∪ xF) ∩ (yE ∪ yF)
        = (xE ∩ (yE ∪ yF)) ∪ (xF ∩ (yE ∪ yF))               [dist.]
        = (xE ∩ yE) ∪ (xE ∩ yF) ∪ (xF ∩ yE) ∪ (xF ∩ yF)     [dist.]

The middle two clauses intersect an elem with a from.  Here, we can
just filter for the elements that are numbers, and are greater than
the from clause:

  e ∩   ∅   = ∅
  e ∩ {i..} = {e | e ≥ i}

The last clause intersects two froms.  Here, we have different cases
depending on whether either of the operands are empty or not:

    ∅   ∪   ∅   = ∅
  {i..} ∪   ∅   = ∅
    ∅   ∪ {j..} = ∅
  {i..} ∪ {j..} = {min(i,j)..}

> intersect :: RangedSet -> RangedSet -> RangedSet
> intersect x y = consolidate $ RangedSet {
>                   elems = xE_yE `Set.union` xE_yF__xF_yE,
>                   from  = xF_yF (from x) (from y)
>                 }
>     where
>       xE_yE = (elems x) `Set.intersection` (elems y)
> 
>       xE_yF = e_f' (elems x) (from y)
>       xF_yE = e_f' (elems y) (from x)
>       xE_yF__xF_yE = xE_yF `Set.union` xF_yE
> 
>       e_f' e Nothing  = Set.empty
>       e_f' e (Just i) = Set.filter (keepAbove i) e
>       keepAbove i (VNumber j) = j >= i
>       keepAbove i _           = False
> 
>       xF_yF Nothing _         = Nothing
>       xF_yF _ Nothing         = Nothing
>       xF_yF (Just i) (Just j) = Just (max i j)


Difference of RangedSets

  x = (xE ∪ xF)
  y = (yE ∪ yF)

To take the difference of two RangedSets, we can use the fake
“negation” operator on sets:

  x ∖ y = (xE ∪ xF) ∖ (yE ∪ yF)
        = (xE ∪ xF) ∩ ¬(yE ∪ yF)                            [defn. of ∖]
        = (xE ∪ xF) ∩ (¬yE ∩ ¬yF)                           [DeMorgan's]
        = (xE ∩ ¬yE ∩ ¬yF) ∪ (xF ∩ ¬yE ∩ ¬yF)               [dist.]
        = ((xE ∖ yE) ∖ yF) ∪ ((xF ∖ yF) ∖ yE)               [defn. of ∖]

In this last step, I've ordered the clauses in that way to allow us to
do the “easy” differences first.  For (xE ∖ yE), we can use the
built-in Set.\\ function.  We can then use a filter to remove yF:

  e ∖   ∅   = e
  e ∖ {i..} = {e | (e < i) ∨ (e isn't a number) }

For (xF ∖ yF), we have:

    ∅   ∖   f   = ∅
  {i..} ∖ {j..} = {i..j-1}
  {i..} ∖   ∅   = {i..}

Unfortunately, the last equation (which creates a from clause) has to
be treated differently than the first two (which create elems clauses)
when removing yE.  For the first two, we can simply use Set.\\, as
above.  For the last, however, we have:

  {i..} ∖ e

To calculate this, we must find the largest number in e.  Everything
above this in the open range is still there; everything below gives us
a regular set that we can use Set.\\ on:

  {i..} ∖ e = ({i..max(e)} ∖ e) ∪ {max(e)+1..}

> difference :: RangedSet -> RangedSet -> RangedSet
> difference x y = consolidate $ RangedSet {
>                    elems = xE_yE_yF (from y) `Set.union` xF_yF_e,
>                    from  = xF_yF_f
>                  }
>     where
>       closedNumSet i j = Set.fromList $ map VNumber [i..j]
>
>       xE_yE = (elems x) Set.\\ (elems y)
>
>       xE_yE_yF Nothing  = xE_yE
>       xE_yE_yF (Just i) = Set.filter (keepBelow i) xE_yE
>       keepBelow i (VNumber j) = j < i
>       keepBelow i _           = True
>
>       (xF_yF_e, xF_yF_f) = xF_yF (from x) (from y)
>
>       xF_yF Nothing _ = (Set.empty, Nothing)
>
>       xF_yF (Just xF) Nothing = (xF_yF_e, xF_yF_f)
>           where
>             yE_nums = Set.filter keepNums (elems y)
>             keepNums (VNumber _) = True
>             keepNums _           = False
>
>             yE_ints = Set.map coerceNumber yE_nums
>             yE_max  = Set.findMax yE_ints
>
>             xF_yF_e = (closedNumSet xF yE_max) Set.\\ yE_nums
>
>             xF_yF_f = Just (yE_max + 1)
>
>       xF_yF (Just xF) (Just yF) = (xF_yF' Set.\\ (elems y), Nothing)
>           where
>             xF_yF' = closedNumSet xF (yF-1)


Distributed union

> distUnion :: RangedSet -> RangedSet
> distUnion rss = foldr union emptyRangedSet $ map coerceSet $ toList rss


Distributed intersection

> distIntersect :: RangedSet -> RangedSet
> distIntersect rss | null rss  = emptyRangedSet
>                   | otherwise = foldr1 intersect $ map coerceSet $ toList rss


Powerset

> powerset :: RangedSet -> RangedSet
> powerset rss = fromList $
>                map (VSet . fromList) $
>                listPowerset $ toList rss

> listPowerset :: [Value] -> [[Value]]
> listPowerset []     = [[]]
> listPowerset (x:xs) = ps ++ map (x:) ps
>     where
>       ps = listPowerset xs


Sequenceset

> {-
> sequenceset :: RangedSet -> RangedSet
> sequenceset rss = fromList $
>                map (VSet . fromList) $
>                listPowerset $ toList rss
> -}

> {-
> listSequenceset :: [Value] -> [[Value]]
> listSequenceset as = [[]] ++
>                      concat $ map (\xs -> (map (:xs) as))
>                                   (listSequenceset as))
> -}
