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

module HST.CSPM.Types where

import Control.Monad.State
import Data.List (intercalate)
import Data.Map (Map)
import qualified Data.Map as Map

import HST.CSP0
import HST.CSPM.Sets (Set)
import qualified HST.CSPM.Sets as Sets

-- Identifiers

newtype Identifier
    = Identifier String
    deriving (Eq, Ord)

instance Show Identifier where
    show (Identifier s) = s

    -- We don't want to show the [] brackets when showing a list of
    -- identifiers.

    showList []     = showString ""
    showList (x:xs) = shows x . showl xs
                      where
                        showl []     = id
                        showl (x:xs) = showChar ' ' . shows x . showl xs

data Binding
    = Binding Identifier Expression
    deriving (Eq, Ord)

instance Show Binding where
    show (Binding id x) = show id ++ " = " ++ show x

-- Environments

data Env
    = Env {
        name   :: String,
        table  :: Map Identifier Expression,
        parent :: Maybe Env
      }
    deriving (Eq, Ord)

-- Values

data ProcPair = ProcPair Process (ScriptTransformer ())

instance Eq ProcPair where
    (ProcPair p1 _) == (ProcPair p2 _) = p1 == p2

instance Ord ProcPair where
    compare (ProcPair p1 _) (ProcPair p2 _) = compare p1 p2

data LambdaClause = Clause Pattern Expression
                    deriving (Eq, Ord)

instance Show LambdaClause where
    show (Clause p x) = show p ++ " @ " ++ show x

    showList []       = showString "{}"
    showList (x:xs)   = showChar '{' . shows x . showl xs
                        where showl []     = showChar '}'
                              showl (x:xs) = showString "; " . shows x .
                                             showl xs

data Value
    = VBottom
    | VNumber Int
    | VSequence [Value]
    | VSet (Set Value)
    | VBoolean Bool
    | VTuple [Value]
    | VDot [Value]
    | VLambda String Env [LambdaClause]
    | VConstructor Identifier
    | VEvent Event
    | VProcess ProcPair
    deriving (Eq, Ord)

instance Show Value where
    show (VBottom)          = "Bottom"
    show (VNumber i)        = show i
    show (VSequence s)      = "<" ++ show s ++ ">"
    show (VSet s)           = "{" ++ show s ++ "}"
    show (VBoolean b)       = show b
    show (VTuple t)         = "(" ++ show t ++ ")"
    show (VDot d)           = intercalate "." (map show d)
    show (VLambda pfx e cs) = "\\ [" ++ show pfx ++ "] " ++ show cs
    show (VConstructor id)  = "[:" ++ show id ++ ":]"
    show (VEvent a)         = show a

    show (VProcess (ProcPair p _)) = "[proc " ++ show p ++ "]"

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

coerceSet :: Value -> (Set Value)
coerceSet (VSet a) = a

coerceBoolean :: Value -> Bool
coerceBoolean (VBoolean b) = b

coerceTuple :: Value -> [Value]
coerceTuple (VTuple t) = t

coerceDot :: Value -> [Value]
coerceDot (VDot d) = d

coerceEvent :: Value -> Event
coerceEvent (VEvent a) = a

coerceProcess :: Value -> ProcPair
coerceProcess (VProcess pp) = pp

-- A smart constructor for VDots; ensures that dot values are
-- “flattened”.

vDot :: Value -> Value -> Value
vDot (VDot d1) (VDot d2) = VDot (d1 ++ d2)
vDot (VDot d1) y         = VDot (d1 ++ [y])
vDot x         (VDot d2) = VDot (x : d2)
vDot x         y         = VDot [x,y]

-- Patterns

data Pattern
    = PNLit Int
    | PWildcard
    | PIdentifier Identifier
    | PTuple [Pattern]
    | PDot Pattern Pattern
    | PQLit [Pattern]
    | PQConcat Pattern Pattern
    | PSEmpty
    | PSSingleton Pattern
    -- | channel
    -- | dotted
    | PConjunction Pattern Pattern
    deriving (Eq, Ord)

instance Show Pattern where
    show (PNLit i)            = show i
    show (PWildcard)          = "_"
    show (PIdentifier id)     = show id
    show (PTuple pt)          = "(" ++ show pt ++ ")"
    show (PDot p1 p2)         = show p1 ++ "." ++ show p2
    show (PQLit pq)           = "<" ++ show pq ++ ">"
    show (PQConcat p1 p2)     = show p1 ++ "^" ++ show p2
    show (PSEmpty)            = "{}"
    show (PSSingleton p)      = "{" ++ show p ++ "}"
    show (PConjunction p1 p2) = show p1 ++ "@" ++ show p2

    -- We don't want to show the [] brackets when showing a list of
    -- patterns, since we're going to use different brackets depending
    -- on whether the list represents a sequence, set, or tuple.

    showList []     = showString ""
    showList (x:xs) = shows x . showl xs
                      where
                        showl []     = id
                        showl (x:xs) = showChar ',' . shows x . showl xs

type PatternMatch = Maybe [Binding]

-- Top-level definitions

data CSPMScript
    = CSPMScript [Definition]
    deriving Show

data Definition
    = DPatternDefn Pattern Expression
    | DLambdaClause Identifier LambdaClause
    | DLambda Identifier [LambdaClause]
    | DSimpleChannel Identifier
    | DNametype Identifier Expression
    | DDatatype Identifier [DConstructor]
    deriving (Eq, Ord, Show)

data DConstructor
    = DConstructor Identifier
      deriving (Eq, Ord, Show)

constructorValues :: DConstructor -> Expression
constructorValues (DConstructor id) = ESLit [EConstructor id]

constructorsValues :: [DConstructor] -> Expression
constructorsValues cs = ESDistUnion $ ESLit $ map constructorValues cs

constructorBinding :: DConstructor -> Binding
constructorBinding (DConstructor id) = Binding id $ EConstructor id

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
    | ESBool
    | ESInt
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
    | ESTupleProduct [Expression]
    | ESDotProduct Expression Expression
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

    -- Expressions which evaluate to a dot
    | EDot Expression Expression

    -- Expressions which evaluate to a lambda
    | ELambda [LambdaClause]

    -- Expressions which can evaluate to anything
    | EVar Identifier
    | ELet [Definition] Expression
    | EApply Expression [Expression]
    | EQHead Expression
    | EIfThenElse Expression Expression Expression
    | EBound BoundExpression
    | EValue Value
    | EExtractMatch Identifier Pattern Expression

    -- Expressions which evaluate to a constructor
    | EConstructor Identifier

    -- Expressions which evaluate to an event
    | EEvent Event

    -- Expressions which evaluate to a process
    | EStop
    | ESkip
    | EPrefix Expression Expression
    | EExtChoice Expression Expression
    | EIntChoice Expression Expression
    | ETimeout Expression Expression
    | ESeqComp Expression Expression
    | EInterleave Expression Expression
    | EIParallel Expression Expression Expression
    | EAParallel Expression Expression Expression Expression
    | EHide Expression Expression
    -- | ERename Expression Expression
    | ERExtChoice Expression
    | ERIntChoice Expression

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

    show ESBool                  = "Bool"
    show ESInt                   = "Int"
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
    show (ESTupleProduct xs)     = "(:" ++ show xs ++ ":)"
    show (ESDotProduct x y)      = "(:" ++ show x ++ "." ++ show y ++ ":)"

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

    show (EDot x y) = show x ++ "." ++ show y

    show (ELambda cs) = "\\ " ++ show cs

    show (EVar id) = show id
    show (ELet ds x) = "let " ++ show ds ++ " within " ++ show x
    show (EApply x ys) = show x ++ "(" ++ show ys ++ ")"

    show (EQHead x) = "head(" ++ show x ++ ")"
    show (EIfThenElse b x y) = "if (" ++ show b ++ ") then " ++
                               show x ++ " else " ++ show y

    show (EBound be) = show be
    show (EValue v) = "<<value: " ++ show v ++ ">>"

    show (EExtractMatch id p x) = "<<extract " ++ show id ++ " from " ++
                                  show p ++ " = " ++ show x ++ ">>"

    show (EConstructor id) = "[:" ++ show id ++ ":]"

    show (EEvent a) = show a

    show EStop = "STOP"
    show ESkip = "SKIP"
    show (EPrefix a p) = show a ++ " -> " ++ show p
    show (EExtChoice p q) = show p ++ " [] " ++ show q
    show (EIntChoice p q) = show p ++ " |~| " ++ show q
    show (ETimeout p q) = show p ++ " [> " ++ show q
    show (ESeqComp p q) = show p ++ " ; " ++ show q
    show (EInterleave p q) = show p ++ " ||| " ++ show q
    show (EIParallel p alpha q) = show p ++ " [|" ++ show alpha ++
                                  "|] " ++ show q
    show (EAParallel p alpha beta q) = show p ++ " [" ++ show alpha ++
                                       " || " ++ show beta ++ "] " ++ show q
    show (EHide p alpha) = show p ++ " \\ " ++ show alpha
    show (ERExtChoice ps) = "[] " ++ show ps
    show (ERIntChoice ps) = "|~| " ++ show ps

    -- We don't want to show the [] brackets when showing a list of
    -- expressions, since we're going to use different brackets
    -- depending on whether the list represents a sequence, set, or
    -- tuple literal.

    showList []     = showString ""
    showList (x:xs) = shows x . showl xs
                      where
                        showl []     = id
                        showl (x:xs) = showChar ',' . shows x . showl xs

-- Bound expressions

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
    | BSBool
    | BSInt
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
    | BSTupleProduct [BoundExpression]
    | BSDotProduct BoundExpression BoundExpression
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

    -- Expressions which evaluate to a dot
    | BDot BoundExpression BoundExpression

    -- Expressions which evaluate to a lambda
    | BLambda String Env [LambdaClause]  -- yep, that contains Expr, not BoundExpr

    -- Expressions which can evaluate to anything
    | BVar Env Identifier
    | BApply BoundExpression [BoundExpression]
    | BQHead BoundExpression
    | BIfThenElse BoundExpression BoundExpression BoundExpression
    | BValue Value
    | BExtractMatch Identifier Pattern BoundExpression

    -- Expression which can evaluate to a constructor
    | BConstructor Identifier

    -- Expression which can evaluate to an event
    | BEvent Event

    -- Expression which can evaluate to a process
    | BStop
    | BSkip
    | BPrefix Process BoundExpression BoundExpression
    | BExtChoice Process BoundExpression BoundExpression
    | BIntChoice Process BoundExpression BoundExpression
    | BTimeout Process BoundExpression BoundExpression
    | BSeqComp Process BoundExpression BoundExpression
    | BInterleave Process BoundExpression BoundExpression
    | BIParallel Process BoundExpression BoundExpression BoundExpression
    | BAParallel Process BoundExpression BoundExpression BoundExpression BoundExpression
    | BHide Process BoundExpression BoundExpression
    -- | BRename Process BoundExpression BoundExpression
    | BRExtChoice Process BoundExpression
    | BRIntChoice Process BoundExpression

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

    show BSBool                  = "Bool"
    show BSInt                   = "Int"
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
    show (BSTupleProduct xs)     = "(:" ++ show xs ++ ":)"
    show (BSDotProduct x y)      = "(:" ++ show x ++ "." ++ show y ++ ":)"

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

    show (BDot x y) = show x ++ "." ++ show y

    show (BLambda pfx e cs) = "\\ [" ++ pfx ++ "] " ++ show cs

    show (BVar e id) = show id
    show (BApply x ys) = show x ++ "(" ++ show ys ++ ")"
    show (BQHead x) = "head(" ++ show x ++ ")"
    show (BIfThenElse b x y) = "if (" ++ show b ++ ") then " ++
                               show x ++ " else " ++ show y

    show (BValue v) = "<<value: " ++ show v ++ ">>"

    show (BExtractMatch id p x) = "<<extract " ++ show id ++ " from " ++
                                  show p ++ " = " ++ show x ++ ">>"

    show (BConstructor id) = "[:" ++ show id ++ ":]"

    show (BEvent a) = show a

    show BStop = "STOP"
    show BSkip = "SKIP"
    show (BPrefix dest a p) = show dest ++ ": (" ++
                              show a ++ " -> " ++ show p ++ ")"
    show (BExtChoice dest p q) = show dest ++ ": (" ++
                                 show p ++ " [] " ++ show q ++ ")"
    show (BIntChoice dest p q) = show dest ++ ": (" ++
                                 show p ++ " |~| " ++ show q ++ ")"
    show (BTimeout dest p q) = show dest ++ ": (" ++
                               show p ++ " [> " ++ show q ++ ")"
    show (BSeqComp dest p q) = show dest ++ ": (" ++
                               show p ++ " ; " ++ show q ++ ")"
    show (BInterleave dest p q) = show dest ++ ": (" ++
                                  show p ++ " ||| " ++ show q ++ ")"
    show (BIParallel dest p alpha q) = show dest ++ ": (" ++
                                       show p ++ " [|" ++ show alpha ++
                                                "|] " ++ show q ++ ")"
    show (BAParallel dest p alpha beta q) = show dest ++ ": (" ++ show p ++
                                            " [" ++ show alpha ++ " || " ++
                                                 show beta ++ "] " ++ show q ++ ")"
    show (BHide dest p alpha) = show dest ++ ": (" ++
                                show p ++ " \\ " ++ show alpha ++ ")"
    show (BRExtChoice dest ps) = show dest ++ ": (" ++
                                 "[] " ++ show ps ++ ")"
    show (BRIntChoice dest ps) = show dest ++ ": (" ++
                                 "|~| " ++ show ps ++ ")"

    -- We don't want to show the [] brackets when showing a list of
    -- expressions, since we're going to use different brackets
    -- depending on whether the list represents a sequence, set, or
    -- tuple literal.

    showList []     = showString ""
    showList (x:xs) = shows x . showl xs
                      where
                        showl []     = id
                        showl (x:xs) = showChar ',' . shows x . showl xs
