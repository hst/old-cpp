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

module HST.CSPM.Values where

-- Values

data Value
    = VBottom
    | VNumber Int
    | VSequence [Value]
    | VSet [Value]
    | VBoolean Bool
    | VTuple [Value]
    deriving (Eq, Ord)

instance Show Value where
    show (VBottom)       = "Bottom"
    show (VNumber i)     = show i
    show (VSequence s)   = "<" ++ show s ++ ">"
    show (VSet s)        = "{" ++ show s ++ "}"
    show (VBoolean b)    = show b
    show (VTuple t)      = "(" ++ show t ++ ")"

    -- We don't want to show the [] brackets when showing a list of
    -- values, since we're going to use different brackets depending
    -- on whether the list represents a sequence, set, or tuple.

    showList []     = showString ""
    showList (x:xs) = shows x . showl xs
                      where
                        showl []     = id
                        showl (x:xs) = showChar ',' . shows x . showl xs

coerceNumber :: Value -> Int
coerceNumber (VNumber i) = i

coerceSequence :: Value -> [Value]
coerceSequence (VSequence s) = s

coerceSet :: Value -> [Value]
coerceSet (VSet a) = a

coerceBoolean :: Value -> Bool
coerceBoolean (VBoolean b) = b

coerceTuple :: Value -> [Value]
coerceTuple (VTuple t) = t
