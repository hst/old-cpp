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

> import Prelude hiding (filter, map, null)
> import qualified Prelude

> import qualified Data.Set as S


Sets

The predefined Data.Set type is great for representing finite sets,
but it doesn't work for infinite sets.  Therefore, we create our own
Set type.  It contains a “loader list”, which can be infinite and can
contain duplicates.  As we access elements from this list, they're
added to a “storage set”, which allows us to ignore any duplicate
elements that appear in the loader list.

Set operations that work on infinite sets should access the elements
using the toList function.  Operations that only work on finite sets
should use the toSet function.

> data Set a
>     = Set {
>         storage :: S.Set a,
>         loader  :: [a]
>       }

> instance (Show a, Ord a) => Show (Set a) where
>     show set = show $ toList set

> instance (Eq a, Ord a) => Eq (Set a) where
>     set1 == set2 = toSet set1 == toSet set2

> instance Ord a => Ord (Set a) where
>     compare set1 set2 = compare (toSet set1) (toSet set2)


The empty ranged set.

> empty :: Set a
> empty = Set { storage = S.empty, loader = [] }


Create a new Set from the given list of elements.

> fromList :: [a] -> Set a
> fromList as = Set {
>                 storage = S.empty,
>                 loader  = as
>               }


Exhausting a set

The exhaust function loads the entire contents of the loader list into
the storage set.  The loader list must be finite.

> toSet :: Ord a => Set a -> S.Set a
> toSet = storage . exhaust

> exhaust :: Ord a => Set a -> Set a
> exhaust (Set s l)
>     = Set {
>         storage = s `S.union` S.fromList l,
>         loader  = []
>       }


Extracting a list from a set

The toList function allows us to work with infinite sets.  The loader
list is only retrieved as needed, much like an infinite list.  At the
same time, we stash away each element as its read into the storage
set, allowing us to ensure that each element is only returned once.
Any elements that have already been loaded into the storage set *will
not* be returned.

The next method is the workhorse of the toList method; it extracts one
element from the loader list, adding it to the storage set.  If this
element wasn't already in the storage set, it's returned; otherwise,
we return Nothing.  We also return the new Set, which will have a
loader list that is one element shorter, and a storage set that might
be one element larger.  Like the head function, there must be at least
one element in the loader list.

> next :: Ord a => Set a -> (Maybe a, Set a)
> next (Set s l)
>     | h `S.member` s = (Nothing, (Set s  l'))
>     | otherwise      = (Just h,  (Set s' l'))
>     where
>       h  = head l
>       s' = S.insert h s
>       l' = tail l

> toList :: Ord a => Set a -> [a]
> toList (Set _ []) = []
> toList set        = case next set of
>                       (Nothing, set') -> toList set'
>                       (Just a, set')  -> a : toList set'


Basic set operations

> member :: Ord a => a -> Set a -> Bool
> member a set@(Set s _) = a `S.member` s || a `elem` toList set

> null :: Set a -> Bool
> null (Set s l) = S.null s && Prelude.null l

> size :: Ord a => Set a -> Int
> size = S.size . toSet

> union :: Ord a => Set a -> Set a -> Set a
> union (Set s1 l1) (Set s2 l2)
>     = Set (s1 `S.union` s2) (alternate l1 l2)
>
> alternate :: [a] -> [a] -> [a]
> alternate [] ys     = ys
> alternate xs []     = xs
> alternate (x:xs) ys = x : alternate ys xs

> mapExhaust2 :: Ord a =>
>                (S.Set a -> S.Set a -> S.Set a) ->
>                Set a -> Set a -> Set a
> mapExhaust2 f set1 set2
>     = Set (f es1 es2) []
>     where
>       es1 = toSet set1
>       es2 = toSet set2

> intersect :: Ord a => Set a -> Set a -> Set a
> intersect = mapExhaust2 S.intersection

> difference :: Ord a => Set a -> Set a -> Set a
> difference = mapExhaust2 S.difference


Filtering

> filter :: Ord a => (a -> Bool) -> Set a -> Set a
> filter f (Set s l) = Set (S.filter f s) (Prelude.filter f l)


Mapping

> map :: (Ord a, Ord b) => (a -> b) -> Set a -> Set b
> map f (Set s l) = Set (S.map f s) (Prelude.map f l)


Distributed union

> distUnion :: Ord a => Set (Set a) -> Set a
> distUnion sset = foldr union empty $ toList sset


Distributed intersection

> distIntersect :: Ord a => Set (Set a) -> Set a
> distIntersect sset | null sset = empty
>                    | otherwise = foldr1 intersect $ toList sset


Powerset

> powerset :: Ord a => Set a -> Set (Set a)
> powerset = fromList . (Prelude.map fromList) . listPowerset . toList

> listPowerset :: [a] -> [[a]]
> listPowerset []     = [[]]
> listPowerset (x:xs) = ps ++ Prelude.map (x:) ps
>     where
>       ps = listPowerset xs


Sequenceset

> sequenceset :: Ord a => Set a -> (Set [a])
> sequenceset rss = fromList $
>                listSequenceset $ toList rss

> listSequenceset :: [a] -> [[a]]
> listSequenceset as = [[]] ++
>                      (concat $ Prelude.map (\xs -> (Prelude.map (:xs) as))
>                                  (listSequenceset as))
