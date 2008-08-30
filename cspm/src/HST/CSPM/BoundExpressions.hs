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

module HST.CSPM.BoundExpressions where

import HST.CSPM.Expressions
import HST.CSPM.Environments

data BoundExpression
    = BBottom

    -- Expressions which evaluate to a number
    | BNLit Int
    | BNNeg BoundExpression
    | BNSum BoundExpression BoundExpression
    | BNDiff BoundExpression BoundExpression
    | BNProd BoundExpression BoundExpression
    | BNQuot BoundExpression BoundExpression
    | BNRem BoundExpression BoundExpression
    | BQLength BoundExpression
    | BSCardinality BoundExpression

    -- Expressions which evaluate to a sequence
    | BQLit [BoundExpression]
    | BQClosedRange BoundExpression BoundExpression
    | BQOpenRange BoundExpression
    | BQConcat BoundExpression BoundExpression
    | BQTail BoundExpression
    -- TODO: sequence comprehension

    -- Expressions which evaluate to a set
    | BSLit [BoundExpression]
    | BSClosedRange BoundExpression BoundExpression
    | BSOpenRange BoundExpression
    | BSUnion BoundExpression BoundExpression
    | BSIntersection BoundExpression BoundExpression
    | BSDifference BoundExpression BoundExpression
    | BSDistUnion BoundExpression
    | BSDistIntersection BoundExpression
    | BQSet BoundExpression
    | BSPowerset BoundExpression
    | BSSequenceset BoundExpression
    -- TODO: set comprehension

    -- Expressions which evaluate to a boolean
    | BBTrue
    | BBFalse
    | BBAnd BoundExpression BoundExpression
    | BBOr BoundExpression BoundExpression
    | BBNot BoundExpression
    | BEqual BoundExpression BoundExpression
    | BNotEqual BoundExpression BoundExpression
    | BLT BoundExpression BoundExpression
    | BGT BoundExpression BoundExpression
    | BLTE BoundExpression BoundExpression
    | BGTE BoundExpression BoundExpression
    | BQEmpty BoundExpression
    | BQIn BoundExpression BoundExpression
    | BSIn BoundExpression BoundExpression
    | BSEmpty BoundExpression

    -- Expressions which evaluate to a tuple
    | BTLit [BoundExpression]

    -- Expressions which can evaluate to anything
    | BVar Env Identifier
    | BQHead BoundExpression
    | BIfThenElse BoundExpression BoundExpression BoundExpression

    deriving (Eq, Ord)


instance Show BoundExpression where
    show BBottom = "Bottom"

    show (BNLit i)         = show i
    show (BNNeg m)         = "(-" ++ show m ++ ")"
    show (BNSum m n)       = "(" ++ show m ++ " + " ++ show n ++ ")"
    show (BNDiff m n)      = "(" ++ show m ++ " - " ++ show n ++ ")"
    show (BNProd m n)      = "(" ++ show m ++ " * " ++ show n ++ ")"
    show (BNQuot m n)      = "(" ++ show m ++ " / " ++ show n ++ ")"
    show (BNRem m n)       = "(" ++ show m ++ " % " ++ show n ++ ")"
    show (BQLength s)      = "(#" ++ show s ++ ")"
    show (BSCardinality a) = "(#" ++ show a ++ ")"

    show (BQLit xs)          = "<" ++ show xs ++ ">"
    show (BQClosedRange m n) = "<" ++ show m ++ ".." ++ show n ++ ">"
    show (BQOpenRange m)     = "<" ++ show m ++ "..>"
    show (BQConcat s t)      = "concat(" ++ show s ++ ", " ++ show t ++ ")"
    show (BQTail s)          = "tail(" ++ show s ++ ")"

    show (BSLit xs)              = "{" ++ show xs ++ "}"
    show (BSClosedRange m n)     = "{" ++ show m ++ ".." ++ show n ++ "}"
    show (BSOpenRange m)         = "{" ++ show m ++ "..}"
    show (BSUnion s1 s2)         = "union(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (BSIntersection s1 s2)  = "inter(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (BSDifference s1 s2)    = "diff(" ++ show s1 ++ ", " ++ show s2 ++ "}"
    show (BSDistUnion s1)        = "Union(" ++ show s1 ++ ")"
    show (BSDistIntersection s1) = "Inter(" ++ show s1 ++ ")"
    show (BQSet q0)              = "set(" ++ show q0 ++ ")"
    show (BSPowerset s1)         = "Set(" ++ show s1 ++ ")"
    show (BSSequenceset s1)      = "Seq(" ++ show s1 ++ ")"

    show BBTrue            = "true"
    show BBFalse           = "false"
    show (BBAnd b1 b2)     = "(" ++ show b1 ++ " && " ++ show b2 ++ ")"
    show (BBOr b1 b2)      = "(" ++ show b1 ++ " || " ++ show b2 ++ ")"
    show (BBNot b1)        = "(!" ++ show b1 ++ ")"
    show (BEqual e1 e2)    = "(" ++ show e1 ++ " == " ++ show e2 ++ ")"
    show (BNotEqual e1 e2) = "(" ++ show e1 ++ " != " ++ show e2 ++ ")"
    show (BLT e1 e2)       = "(" ++ show e1 ++ " < " ++ show e2 ++ ")"
    show (BGT e1 e2)       = "(" ++ show e1 ++ " > " ++ show e2 ++ ")"
    show (BLTE e1 e2)      = "(" ++ show e1 ++ " <= " ++ show e2 ++ ")"
    show (BGTE e1 e2)      = "(" ++ show e1 ++ " >= " ++ show e2 ++ ")"
    show (BQEmpty q0)      = "null(" ++ show q0 ++ ")"
    show (BQIn x q0)       = "elem(" ++ show x ++ ", " ++ show q0 ++ ")"
    show (BSIn x s0)       = "member(" ++ show x ++ ", " ++ show s0 ++ ")"
    show (BSEmpty s0)      = "empty(" ++ show s0 ++ ")"

    show (BTLit xs) = "(" ++ show xs ++ ")"

    show (BVar e id) = show id

    show (BQHead x) = "head(" ++ show x ++ ")"
    show (BIfThenElse b x y) = "if (" ++ show b ++ ") then " ++
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



bind :: Env -> Expression -> BoundExpression

bind e EBottom = BBottom

-- Numbers

bind e (ENLit i)         = BNLit i
bind e (ENNeg x)         = BNNeg (bind e x)
bind e (ENSum x y)       = BNSum (bind e x) (bind e y)
bind e (ENDiff x y)      = BNDiff (bind e x) (bind e y)
bind e (ENProd x y)      = BNProd (bind e x) (bind e y)
bind e (ENQuot x y)      = BNQuot (bind e x) (bind e y)
bind e (ENRem x y)       = BNRem (bind e x) (bind e y)
bind e (EQLength x)      = BQLength (bind e x)
bind e (ESCardinality x) = BSCardinality (bind e x)

-- Sequences

bind e (EQLit xs)          = BQLit (map (bind e) xs)
bind e (EQClosedRange x y) = BQClosedRange (bind e x) (bind e y)
bind e (EQOpenRange x)     = BQOpenRange (bind e x)
bind e (EQConcat x y)      = BQConcat (bind e x) (bind e y)
bind e (EQTail x)          = BQTail (bind e x)

-- Sets

bind e (ESLit xs)             = BSLit (map (bind e) xs)
bind e (ESClosedRange x y)    = BSClosedRange (bind e x) (bind e y)
bind e (ESOpenRange x)        = BSOpenRange (bind e x)
bind e (ESUnion x y)          = BSUnion (bind e x) (bind e y)
bind e (ESIntersection x y)   = BSIntersection (bind e x) (bind e y)
bind e (ESDifference x y)     = BSDifference (bind e x) (bind e y)
bind e (ESDistUnion x)        = BSDistUnion (bind e x)
bind e (ESDistIntersection x) = BSDistIntersection (bind e x)
bind e (EQSet x)              = BQSet (bind e x)
bind e (ESPowerset x)         = BSPowerset (bind e x)
bind e (ESSequenceset x)      = BSSequenceset (bind e x)

-- Booleans

bind e EBTrue          = BBTrue
bind e EBFalse         = BBFalse
bind e (EBAnd x y)     = BBAnd (bind e x) (bind e y)
bind e (EBOr x y)      = BBOr (bind e x) (bind e y)
bind e (EBNot x)       = BBNot (bind e x)
bind e (EEqual x y)    = BEqual (bind e x) (bind e y)
bind e (ENotEqual x y) = BNotEqual (bind e x) (bind e y)
bind e (ELT x y)       = BLT (bind e x) (bind e y)
bind e (EGT x y)       = BGT (bind e x) (bind e y)
bind e (ELTE x y)      = BLTE (bind e x) (bind e y)
bind e (EGTE x y)      = BGTE (bind e x) (bind e y)
bind e (EQEmpty x)     = BQEmpty (bind e x)
bind e (EQIn x y)      = BQIn (bind e x) (bind e y)
bind e (ESIn x y)      = BSIn (bind e x) (bind e y)
bind e (ESEmpty x)     = BSEmpty (bind e x)

-- Tuples

bind e (ETLit xs) = BTLit (map (bind e) xs)

-- Anything

bind e (EQHead x)          = BQHead (bind e x)
bind e (EIfThenElse x y z) = BIfThenElse (bind e x) (bind e y) (bind e z)

bind e (EVar id) = BVar e id

bind e (ELet bs x) = bind e1 x
    where
      e1 = extendEnv e bs
