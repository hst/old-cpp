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

-- Identifiers

newtype Identifier
    = Identifier String
    deriving (Eq, Ord)

instance Show Identifier where
    show (Identifier s) = s

data Binding
    = Binding Identifier Expression
    deriving (Eq, Ord)

instance Show Binding where
    show (Binding id x) = show id ++ " = " ++ show x

-- Expressions

data Expression
    = EBottom

    -- Expressions which evaluate to a number
    | ENLit Int
    | ENNeg Expression
    | ENSum Expression Expression
    | ENDiff Expression Expression
    | ENProd Expression Expression
    | ENQuot Expression Expression
    | ENRem Expression Expression
    | EQLength Expression
    | ESCardinality Expression

    -- Expressions which evaluate to a sequence
    | EQLit [Expression]
    | EQClosedRange Expression Expression
    | EQOpenRange Expression
    | EQConcat Expression Expression
    | EQTail Expression
    -- TODO: sequence comprehension

    -- Expressions which evaluate to a set
    | ESLit [Expression]
    | ESClosedRange Expression Expression
    | ESOpenRange Expression
    | ESUnion Expression Expression
    | ESIntersection Expression Expression
    | ESDifference Expression Expression
    | ESDistUnion Expression
    | ESDistIntersection Expression
    | EQSet Expression
    | ESPowerset Expression
    | ESSequenceset Expression
    -- TODO: set comprehension

    -- Expressions which evaluate to a boolean
    | EBTrue
    | EBFalse
    | EBAnd Expression Expression
    | EBOr Expression Expression
    | EBNot Expression
    | EEqual Expression Expression
    | ENotEqual Expression Expression
    | ELT Expression Expression
    | EGT Expression Expression
    | ELTE Expression Expression
    | EGTE Expression Expression
    | EQEmpty Expression
    | EQIn Expression Expression
    | ESIn Expression Expression
    | ESEmpty Expression

    -- Expressions which evaluate to a tuple
    | ETLit [Expression]

    -- Expressions which can evaluate to anything
    | EVar Identifier
    | ELet [Binding] Expression
    | EQHead Expression
    | EIfThenElse Expression Expression Expression

    deriving (Eq, Ord)

instance Show Expression where
    show EBottom = "Bottom"

    show (ENLit i)         = show i
    show (ENNeg m)         = "(-" ++ show m ++ ")"
    show (ENSum m n)       = "(" ++ show m ++ " + " ++ show n ++ ")"
    show (ENDiff m n)      = "(" ++ show m ++ " - " ++ show n ++ ")"
    show (ENProd m n)      = "(" ++ show m ++ " * " ++ show n ++ ")"
    show (ENQuot m n)      = "(" ++ show m ++ " / " ++ show n ++ ")"
    show (ENRem m n)       = "(" ++ show m ++ " % " ++ show n ++ ")"
    show (EQLength s)      = "(#" ++ show s ++ ")"
    show (ESCardinality a) = "(#" ++ show a ++ ")"

    show (EQLit xs)          = "<" ++ show xs ++ ">"
    show (EQClosedRange m n) = "<" ++ show m ++ ".." ++ show n ++ ">"
    show (EQOpenRange m)     = "<" ++ show m ++ "..>"
    show (EQConcat s t)      = "concat(" ++ show s ++ ", " ++ show t ++ ")"
    show (EQTail s)          = "tail(" ++ show s ++ ")"

    show (ESLit xs)              = "{" ++ show xs ++ "}"
    show (ESClosedRange m n)     = "{" ++ show m ++ ".." ++ show n ++ "}"
    show (ESOpenRange m)         = "{" ++ show m ++ "..}"
    show (ESUnion s1 s2)         = "union(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (ESIntersection s1 s2)  = "inter(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (ESDifference s1 s2)    = "diff(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (ESDistUnion s1)        = "Union(" ++ show s1 ++ ")"
    show (ESDistIntersection s1) = "Inter(" ++ show s1 ++ ")"
    show (EQSet q0)              = "set(" ++ show q0 ++ ")"
    show (ESPowerset s1)         = "Set(" ++ show s1 ++ ")"
    show (ESSequenceset s1)      = "Seq(" ++ show s1 ++ ")"

    show EBTrue            = "true"
    show EBFalse           = "false"
    show (EBAnd b1 b2)     = "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
    show (EBOr b1 b2)      = "(" ++ show b1 ++ " || " ++ show b2 ++ ")"
    show (EBNot b1)        = "(!" ++ show b1 ++ ")"
    show (EEqual e1 e2)    = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (ENotEqual e1 e2) = "(" ++ show e1 ++ " != " ++ show e2 ++ ")"
    show (ELT e1 e2)       = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (EGT e1 e2)       = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"
    show (ELTE e1 e2)      = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
    show (EGTE e1 e2)      = "(" ++ show e1 ++ " >= " ++ show e2 ++ ")"
    show (EQEmpty q0)      = "null(" ++ show q0 ++ ")"
    show (EQIn x q0)       = "elem(" ++ show x ++ ", " ++ show q0 ++ ")"
    show (ESIn x s0)       = "member(" ++ show x ++ ", " ++ show s0 ++ ")"
    show (ESEmpty s0)      = "empty(" ++ show s0 ++ ")"

    show (ETLit xs) = "(" ++ show xs ++ ")"

    show (EVar id) = show id
    show (ELet bs x) = "let " ++ show bs ++ " within " ++ show x
    show (EQHead x) = "head(" ++ show x ++ ")"
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
