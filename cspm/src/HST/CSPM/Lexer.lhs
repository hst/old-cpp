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

> module HST.CSPM.Lexer
>     (
>      Token(..), lexer
>     )
>     where

> import Char

> data Token
>     = TLParen
>     | TRParen
>     | TLBrace
>     | TRBrace
>     | TLBracket
>     | TRBracket
>     | TLAngle
>     | TRAngle
>     | TBackslash
>     | TPipe
>     | TColon
>     | TAmpersand
>     | TSemi
>     | TDot
>     | TQMark
>     | TBang
>     | TAt
>     | TComma
>     | TEquals
>     | TPlus
>     | TMinus
>     | TAsterisk
>     | TSlash
>     | TPercent
>     | THash
>     | TCaret
>     | TLMap
>     | TRMap
>     | TLPBrace
>     | TRPBrace
>     | TLPar
>     | TRPar
>     | TEq
>     | TNE
>     | TLE
>     | TGE
>     | TRTriangle
>     | TBox
>     | TTwoPipe
>     | TLArrow
>     | TRArrow
>     | TTriangle
>     | TTwoDot
>     | TCap
>     | TThreePipe
>     | TRefinedBy String
>     | TAnd
>     | TAssert
>     | TChannel
>     | TChaos
>     | TDatatype
>     | TElse
>     | TExternal
>     | TFalse
>     | TIf
>     | TLet
>     | TNot
>     | TOr
>     | TPragma
>     | TPrint
>     | TSkip
>     | TStop
>     | TThen
>     | TTransparent
>     | TTrue
>     | TWithin
>     | TIdentifier String
>     | TNumber Int
>     | TNewline
>     | TBadChar
>     deriving (Eq, Ord, Show)

> lexer :: String -> [Token]

> lexer [] = []

> lexer ('\n':cs) = TNewline : lexer cs

> lexer ('|':'~':'|':cs) = TCap : lexer cs
> lexer ('|':'|':'|':cs) = TThreePipe : lexer cs

> lexer ('[':'[':cs) = TLMap : lexer cs
> lexer (']':']':cs) = TRMap : lexer cs
> lexer ('{':'|':cs) = TLPBrace : lexer cs
> lexer ('|':'}':cs) = TRPBrace : lexer cs
> lexer ('[':'|':cs) = TLPar : lexer cs
> lexer ('|':']':cs) = TRPar : lexer cs
> lexer ('=':'=':cs) = TEq : lexer cs
> lexer ('!':'=':cs) = TNE : lexer cs
> lexer ('<':'=':cs) = TLE : lexer cs
> lexer ('>':'=':cs) = TGE : lexer cs
> lexer ('[':'>':cs) = TRTriangle : lexer cs
> lexer ('[':']':cs) = TBox : lexer cs
> lexer ('|':'|':cs) = TTwoPipe : lexer cs
> lexer ('<':'-':cs) = TLArrow : lexer cs
> lexer ('-':'>':cs) = TRArrow : lexer cs
> lexer ('/':'\\':cs) = TTriangle : lexer cs
> lexer ('.':'.':cs) = TTwoDot : lexer cs

> lexer ('(':cs) = TLParen : lexer cs
> lexer (')':cs) = TRParen : lexer cs
> lexer ('{':cs) = TLBrace : lexer cs
> lexer ('}':cs) = TRBrace : lexer cs
> lexer ('[':cs) = TLBracket : lexer cs
> lexer (']':cs) = TRBracket : lexer cs
> lexer ('<':cs) = TLAngle : lexer cs
> lexer ('>':cs) = TRAngle : lexer cs
> lexer ('\\':cs) = TBackslash : lexer cs
> lexer ('|':cs) = TPipe : lexer cs
> lexer (':':cs) = TColon : lexer cs
> lexer ('&':cs) = TAmpersand : lexer cs
> lexer (';':cs) = TSemi : lexer cs
> lexer ('.':cs) = TDot : lexer cs
> lexer ('?':cs) = TQMark : lexer cs
> lexer ('!':cs) = TBang : lexer cs
> lexer ('@':cs) = TAt : lexer cs
> lexer (',':cs) = TComma : lexer cs
> lexer ('=':cs) = TEquals : lexer cs
> lexer ('+':cs) = TPlus : lexer cs
> lexer ('-':cs) = TMinus : lexer cs
> lexer ('*':cs) = TAsterisk : lexer cs
> lexer ('/':cs) = TSlash : lexer cs
> lexer ('%':cs) = TPercent : lexer cs
> lexer ('#':cs) = THash : lexer cs
> lexer ('^':cs) = TCaret : lexer cs

> --lexer ('[':'=':cs) = TRefinedBy "T" : lexer cs

> lexer cs@(c:cs')
>     | isSpace c = lexer cs'
>     | isAlpha c = lexIDOrKeyword cs
>     | isDigit c = lexDigit cs
>     | otherwise = [TBadChar]

> isIDChar :: Char -> Bool
> isIDChar c = c `elem` ("ABCDEFGHIJKLMNOPQRSTUVWXYZ\
>                        \abcdefghijklmnopqrstuvwxyz\
>                        \0123456789_")

> lexID :: String -> (String, String)
> lexID cs@(c:cs') = (alphas ++ primes, rest2)
>     where
>       (alphas, rest1) = span isIDChar cs
>       (primes, rest2) = span (== '\'') rest1

> keyword :: String -> Token
> keyword "and"         = TAnd
> keyword "assert"      = TAssert
> keyword "channel"     = TChannel
> keyword "chaos"       = TChaos
> keyword "datatype"    = TDatatype
> keyword "else"        = TElse
> keyword "external"    = TExternal
> keyword "false"       = TFalse
> keyword "if"          = TIf
> keyword "let"         = TLet
> keyword "not"         = TNot
> keyword "or"          = TOr
> keyword "pragma"      = TPragma
> keyword "print"       = TPrint
> keyword "SKIP"        = TSkip
> keyword "STOP"        = TStop
> keyword "then"        = TThen
> keyword "transparent" = TTransparent
> keyword "true"        = TTrue
> keyword "within"      = TWithin
> keyword id            = TIdentifier id

> lexIDOrKeyword :: String -> [Token]
> lexIDOrKeyword cs = keyword id : lexer rest
>     where
>       (id, rest) = lexID cs

> lexDigit :: String -> [Token]
> lexDigit cs = TNumber (read digits) : lexer rest
>     where
>       (digits, rest) = span isDigit cs