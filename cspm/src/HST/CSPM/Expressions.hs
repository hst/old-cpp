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

module HST.CSPM.Expressions where

-- Expressions

data Expression
    = ENumber Number
    | ESequence Sequence
    | ESet Set
    | EBoolean Boolean
    | ETuple Tuple
    | QHead Expression
    | EIfThenElse Expression Expression Expression

instance Show Expression where
    show (ENumber n) = show n
    show (ESequence s) = show s
    show (ESet a) = show a
    show (EBoolean b) = show b
    show (ETuple t) = show t
    show (QHead x) = "head(" ++ show x ++ ")"
    show (EIfThenElse b x y) = "if (" ++ show b ++ ") then " ++
                               show x ++ " else " ++ show y

    -- We don't want to show the [] brackets when showing a list of
    -- expressions, since we're going to use different brackets
    -- depending on whether the list represents a sequence, set, or
    -- tuple literal.

    showList []     = showString ""
    showList (x:xs) = shows x . showl xs
                      where
                        showl []     = id
                        showl (x:xs) = showChar ',' . shows x . showl xs

-- Numbers

data Number
    = NLit Int
    | NNeg Expression
    | NSum Expression Expression
    | NDiff Expression Expression
    | NProd Expression Expression
    | NQuot Expression Expression
    | NRem Expression Expression
    | QLength Expression
    | SCardinality Expression

instance Show Number where
    show (NLit i)         = show i
    show (NNeg m)         = "(-" ++ show m ++ ")"
    show (NSum m n)       = "(" ++ show m ++ " + " ++ show n ++ ")"
    show (NDiff m n)      = "(" ++ show m ++ " - " ++ show n ++ ")"
    show (NProd m n)      = "(" ++ show m ++ " * " ++ show n ++ ")"
    show (NQuot m n)      = "(" ++ show m ++ " / " ++ show n ++ ")"
    show (NRem m n)       = "(" ++ show m ++ " % " ++ show n ++ ")"
    show (QLength s)      = "(#" ++ show s ++ ")"
    show (SCardinality a) = "(#" ++ show a ++ ")"

-- Sequences

data Sequence
    = QLit [Expression]
    | QClosedRange Expression Expression
    | QOpenRange Expression
    | QConcat Expression Expression
    | QTail Expression
    -- TODO: sequence comprehension

instance Show Sequence where
    show (QLit xs)          = "<" ++ show xs ++ ">"
    show (QClosedRange m n) = "<" ++ show m ++ ".." ++ show n ++ ">"
    show (QOpenRange m)     = "<" ++ show m ++ "..>"
    show (QConcat s t)      = "concat(" ++ show s ++ ", " ++ show t ++ ")"
    show (QTail s)          = "tail(" ++ show s ++ ")"

-- Sets

data Set
    = SLit [Expression]
    | SClosedRange Expression Expression
    | SOpenRange Expression
    | SUnion Expression Expression
    | SIntersection Expression Expression
    | SDifference Expression Expression
    | SDistUnion Expression
    | SDistIntersection Expression
    | QSet Expression
    | SPowerset Expression
    | SSequenceset Expression
    -- TODO: set comprehension

instance Show Set where
    show (SLit xs)              = "{" ++ show xs ++ "}"
    show (SClosedRange m n)     = "{" ++ show m ++ ".." ++ show n ++ "}"
    show (SOpenRange m)         = "{" ++ show m ++ "..}"
    show (SUnion s1 s2)         = "union(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (SIntersection s1 s2)  = "inter(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (SDifference s1 s2)    = "diff(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (SDistUnion s1)        = "Union(" ++ show s1 ++ ")"
    show (SDistIntersection s1) = "Inter(" ++ show s1 ++ ")"
    show (QSet q0)              = "set(" ++ show q0 ++ ")"
    show (SPowerset s1)         = "Set(" ++ show s1 ++ ")"
    show (SSequenceset s1)      = "Seq(" ++ show s1 ++ ")"

-- Booleans

data Boolean
    = BTrue
    | BFalse
    | BAnd Expression Expression
    | BOr Expression Expression
    | BNot Expression
    | EEqual Expression Expression
    | ENotEqual Expression Expression
    | ELT Expression Expression
    | EGT Expression Expression
    | ELTE Expression Expression
    | EGTE Expression Expression
    | QEmpty Expression
    | QIn Expression Expression
    | SIn Expression Expression
    | SEmpty Expression

instance Show Boolean where
    show BTrue             = "true"
    show BFalse            = "false"
    show (BAnd b1 b2)      = "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
    show (BOr b1 b2)       = "(" ++ show b1 ++ " || " ++ show b2 ++ ")"
    show (BNot b1)         = "(!" ++ show b1 ++ ")"
    show (EEqual e1 e2)    = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (ENotEqual e1 e2) = "(" ++ show e1 ++ " != " ++ show e2 ++ ")"
    show (ELT e1 e2)       = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (EGT e1 e2)       = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"
    show (ELTE e1 e2)      = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
    show (EGTE e1 e2)      = "(" ++ show e1 ++ " >= " ++ show e2 ++ ")"
    show (QEmpty q0)       = "null(" ++ show q0 ++ ")"
    show (QIn x q0)        = "elem(" ++ show x ++ ", " ++ show q0 ++ ")"
    show (SIn x s0)        = "member(" ++ show x ++ ", " ++ show s0 ++ ")"
    show (SEmpty s0)       = "empty(" ++ show s0 ++ ")"

-- Tuples

data Tuple
    = TLit [Expression]
    deriving Show
