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
>     | TWildcard
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
>     | TBool
>     | TCard
>     | TChannel
>     | TChaos
>     | TDatatype
>     | TDiff
>     | TDInter
>     | TDUnion
>     | TElem
>     | TElse
>     | TEmpty
>     | TExternal
>     | TFalse
>     | THead
>     | TIf
>     | TInt
>     | TInter
>     | TLength
>     | TLet
>     | TMember
>     | TNametype
>     | TNot
>     | TNull
>     | TOr
>     | TPowerset
>     | TPragma
>     | TPrint
>     | TSequenceset
>     | TSet
>     | TSkip
>     | TStop
>     | TTail
>     | TThen
>     | TTransparent
>     | TTrue
>     | TUnion
>     | TWithin
>     | TIdentifier String
>     | TNumber Int
>     | TNewline
>     | TBadChar
>     deriving (Eq, Ord, Show)

The base lexer.  We have to do some postprocessing to the token
stream, however, so this is not the “lexer” function that's exported
from the module.

> baseLexer :: String -> [Token]

> baseLexer [] = []

> baseLexer ('\n':cs) = TNewline : baseLexer cs

> baseLexer ('-':'-':cs) = soakLineComment cs
> baseLexer ('{':'-':cs) = soakBlockComment cs

> baseLexer ('|':'~':'|':cs) = TCap : baseLexer cs
> baseLexer ('|':'|':'|':cs) = TThreePipe : baseLexer cs

> baseLexer ('[':'[':cs) = TLMap : baseLexer cs
> baseLexer (']':']':cs) = TRMap : baseLexer cs
> baseLexer ('{':'|':cs) = TLPBrace : baseLexer cs
> baseLexer ('|':'}':cs) = TRPBrace : baseLexer cs
> baseLexer ('[':'|':cs) = TLPar : baseLexer cs
> baseLexer ('|':']':cs) = TRPar : baseLexer cs
> baseLexer ('=':'=':cs) = TEq : baseLexer cs
> baseLexer ('!':'=':cs) = TNE : baseLexer cs
> baseLexer ('<':'=':cs) = TLE : baseLexer cs
> baseLexer ('>':'=':cs) = TGE : baseLexer cs
> baseLexer ('[':'>':cs) = TRTriangle : baseLexer cs
> baseLexer ('[':']':cs) = TBox : baseLexer cs
> baseLexer ('|':'|':cs) = TTwoPipe : baseLexer cs
> baseLexer ('<':'-':cs) = TLArrow : baseLexer cs
> baseLexer ('-':'>':cs) = TRArrow : baseLexer cs
> baseLexer ('/':'\\':cs) = TTriangle : baseLexer cs
> baseLexer ('.':'.':cs) = TTwoDot : baseLexer cs

> baseLexer ('(':cs) = TLParen : baseLexer cs
> baseLexer (')':cs) = TRParen : baseLexer cs
> baseLexer ('{':cs) = TLBrace : baseLexer cs
> baseLexer ('}':cs) = TRBrace : baseLexer cs
> baseLexer ('[':cs) = TLBracket : baseLexer cs
> baseLexer (']':cs) = TRBracket : baseLexer cs
> baseLexer ('<':cs) = TLAngle : baseLexer cs
> baseLexer ('>':cs) = TRAngle : baseLexer cs
> baseLexer ('\\':cs) = TBackslash : baseLexer cs
> baseLexer ('|':cs) = TPipe : baseLexer cs
> baseLexer (':':cs) = TColon : baseLexer cs
> baseLexer ('&':cs) = TAmpersand : baseLexer cs
> baseLexer (';':cs) = TSemi : baseLexer cs
> baseLexer ('.':cs) = TDot : baseLexer cs
> baseLexer ('?':cs) = TQMark : baseLexer cs
> baseLexer ('!':cs) = TBang : baseLexer cs
> baseLexer ('@':cs) = TAt : baseLexer cs
> baseLexer (',':cs) = TComma : baseLexer cs
> baseLexer ('=':cs) = TEquals : baseLexer cs
> baseLexer ('+':cs) = TPlus : baseLexer cs
> baseLexer ('-':cs) = TMinus : baseLexer cs
> baseLexer ('*':cs) = TAsterisk : baseLexer cs
> baseLexer ('/':cs) = TSlash : baseLexer cs
> baseLexer ('%':cs) = TPercent : baseLexer cs
> baseLexer ('#':cs) = THash : baseLexer cs
> baseLexer ('^':cs) = TCaret : baseLexer cs
> baseLexer ('_':cs) = TWildcard : baseLexer cs

> --baseLexer ('[':'=':cs) = TRefinedBy "T" : baseLexer cs

> baseLexer cs@(c:cs')
>     | isSpace c = baseLexer cs'
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
> keyword "Bool"        = TBool
> keyword "card"        = TCard
> keyword "channel"     = TChannel
> keyword "chaos"       = TChaos
> keyword "datatype"    = TDatatype
> keyword "diff"        = TDiff
> keyword "Inter"       = TDInter
> keyword "Union"       = TDUnion
> keyword "elem"        = TElem
> keyword "else"        = TElse
> keyword "empty"       = TEmpty
> keyword "external"    = TExternal
> keyword "false"       = TFalse
> keyword "head"        = THead
> keyword "if"          = TIf
> keyword "Int"         = TInt
> keyword "inter"       = TInter
> keyword "length"      = TLength
> keyword "let"         = TLet
> keyword "member"      = TMember
> keyword "nametype"    = TNametype
> keyword "not"         = TNot
> keyword "null"        = TNull
> keyword "or"          = TOr
> keyword "Set"         = TPowerset
> keyword "pragma"      = TPragma
> keyword "print"       = TPrint
> keyword "Seq"         = TSequenceset
> keyword "set"         = TSet
> keyword "SKIP"        = TSkip
> keyword "STOP"        = TStop
> keyword "tail"        = TTail
> keyword "then"        = TThen
> keyword "transparent" = TTransparent
> keyword "true"        = TTrue
> keyword "union"       = TUnion
> keyword "within"      = TWithin
> keyword id            = TIdentifier id

> lexIDOrKeyword :: String -> [Token]
> lexIDOrKeyword cs = keyword id : baseLexer rest
>     where
>       (id, rest) = lexID cs

> lexDigit :: String -> [Token]
> lexDigit cs = TNumber (read digits) : baseLexer rest
>     where
>       (digits, rest) = span isDigit cs


Read the remainder of a single-line comment.  The opening “--” should
already have been read.  The newline that ends the comment will be
returned as a token.  We can match EOF before we read the closing
newline; this just means that this comment closes out the file.

> soakLineComment :: String -> [Token]
> soakLineComment [] = []
> soakLineComment ('\n':cs) = TNewline : baseLexer cs
> soakLineComment (_:cs) = soakLineComment cs


Read the remainder of a block comment.  The opening “{-” should
already have been read.  Any newlines that appear in the comment do
*not* get returned as tokens.  We also have to handle nested block
comments, so if we see another “{-” before seeing the closing “-}”, we
increment a depth counter and keep going.

> soakBlockComment :: String -> [Token]
> soakBlockComment cs = soaker 0 cs
>     where
>       soaker _ [] = [TBadChar]
>       soaker 0 ('-':'}':cs) = baseLexer cs
>       soaker x ('-':'}':cs) = soaker (x-1) cs
>       soaker x ('{':'-':cs) = soaker (x+1) cs
>       soaker x (_:cs)       = soaker x cs


Reduce any sequence of consecutive TNewline tokens into a single
token.

The flag is whether we've just read a newline.

> reduceNewlines :: [Token] -> [Token]
> reduceNewlines ts = fst $ foldr reducer ([], False) ts
>     where
>       reducer TNewline (ts, False) = (TNewline:ts, True)
>       reducer TNewline (ts, True)  = (ts,          True)
>       reducer t        (ts, _)     = (t:ts,        False)


Remove any newlines that appear before a binary or unary operator (a
“soaker”).

The flag is whether we've just read a token that shouldn't be preceded
by a newline.

> soaksNewlinesBefore :: Token -> Bool
> soaksNewlinesBefore TLParen        = False
> soaksNewlinesBefore TRParen        = True
> soaksNewlinesBefore TLBrace        = False
> soaksNewlinesBefore TRBrace        = True
> soaksNewlinesBefore TLBracket      = True
> soaksNewlinesBefore TRBracket      = True
> soaksNewlinesBefore TLAngle        = False
> soaksNewlinesBefore TRAngle        = True
> soaksNewlinesBefore TBackslash     = True
> soaksNewlinesBefore TPipe          = True
> soaksNewlinesBefore TColon         = True
> soaksNewlinesBefore TAmpersand     = True
> soaksNewlinesBefore TSemi          = True
> soaksNewlinesBefore TDot           = True
> soaksNewlinesBefore TQMark         = True
> soaksNewlinesBefore TBang          = True
> soaksNewlinesBefore TAt            = True
> soaksNewlinesBefore TComma         = True
> soaksNewlinesBefore TEquals        = True
> soaksNewlinesBefore TPlus          = True
> soaksNewlinesBefore TMinus         = False
> soaksNewlinesBefore TAsterisk      = True
> soaksNewlinesBefore TSlash         = True
> soaksNewlinesBefore TPercent       = True
> soaksNewlinesBefore THash          = True
> soaksNewlinesBefore TCaret         = True
> soaksNewlinesBefore TLMap          = True
> soaksNewlinesBefore TRMap          = True
> soaksNewlinesBefore TLPBrace       = False
> soaksNewlinesBefore TRPBrace       = True
> soaksNewlinesBefore TLPar          = True
> soaksNewlinesBefore TRPar          = True
> soaksNewlinesBefore TEq            = True
> soaksNewlinesBefore TNE            = True
> soaksNewlinesBefore TLE            = True
> soaksNewlinesBefore TGE            = True
> soaksNewlinesBefore TRTriangle     = True
> soaksNewlinesBefore TBox           = True
> soaksNewlinesBefore TTwoPipe       = True
> soaksNewlinesBefore TLArrow        = True
> soaksNewlinesBefore TRArrow        = True
> soaksNewlinesBefore TTriangle      = True
> soaksNewlinesBefore TTwoDot        = True
> soaksNewlinesBefore TCap           = True
> soaksNewlinesBefore TThreePipe     = True
> soaksNewlinesBefore (TRefinedBy _) = True
> soaksNewlinesBefore TAnd           = True
> soaksNewlinesBefore TElse          = True
> soaksNewlinesBefore TIf            = True
> soaksNewlinesBefore TLet           = True
> soaksNewlinesBefore TNot           = True
> soaksNewlinesBefore TOr            = True
> soaksNewlinesBefore TThen          = True
> soaksNewlinesBefore TWithin        = True
> soaksNewlinesBefore _              = False

> removeNewlinesBeforeSoaker :: [Token] -> [Token]
> removeNewlinesBeforeSoaker ts = fst $ foldr reducer ([], False) ts
>     where
>       reducer TNewline (ts, False) = (TNewline:ts, False)
>       reducer TNewline (ts, True)  = (ts,          False)
>       reducer t        (ts, _)     = (t:ts,        soaksNewlinesBefore t)


Remove any newlines that appear after a binary or unary operator (a
“soaker”).

The flag is whether we've just read a newline.

  rx (t:TNewline:ts) | soaksNewlinesAfter t = t : rx ts
                     | otherwise            = t : TNewline : rx ts
  rx (t:ts)                                 = t : rx ts
  rx []                                     = []

> soaksNewlinesAfter :: Token -> Bool
> soaksNewlinesAfter TLParen        = True
> soaksNewlinesAfter TRParen        = False
> soaksNewlinesAfter TLBrace        = True
> soaksNewlinesAfter TRBrace        = False
> soaksNewlinesAfter TLBracket      = True
> soaksNewlinesAfter TRBracket      = True
> soaksNewlinesAfter TLAngle        = True
> soaksNewlinesAfter TRAngle        = False
> soaksNewlinesAfter TBackslash     = True
> soaksNewlinesAfter TPipe          = True
> soaksNewlinesAfter TColon         = True
> soaksNewlinesAfter TAmpersand     = True
> soaksNewlinesAfter TSemi          = True
> soaksNewlinesAfter TDot           = True
> soaksNewlinesAfter TQMark         = True
> soaksNewlinesAfter TBang          = True
> soaksNewlinesAfter TAt            = True
> soaksNewlinesAfter TComma         = True
> soaksNewlinesAfter TEquals        = True
> soaksNewlinesAfter TPlus          = True
> soaksNewlinesAfter TMinus         = True
> soaksNewlinesAfter TAsterisk      = True
> soaksNewlinesAfter TSlash         = True
> soaksNewlinesAfter TPercent       = True
> soaksNewlinesAfter THash          = True
> soaksNewlinesAfter TCaret         = True
> soaksNewlinesAfter TLMap          = True
> soaksNewlinesAfter TRMap          = False
> soaksNewlinesAfter TLPBrace       = True
> soaksNewlinesAfter TRPBrace       = False
> soaksNewlinesAfter TLPar          = True
> soaksNewlinesAfter TRPar          = True
> soaksNewlinesAfter TEq            = True
> soaksNewlinesAfter TNE            = True
> soaksNewlinesAfter TLE            = True
> soaksNewlinesAfter TGE            = True
> soaksNewlinesAfter TRTriangle     = True
> soaksNewlinesAfter TBox           = True
> soaksNewlinesAfter TTwoPipe       = True
> soaksNewlinesAfter TLArrow        = True
> soaksNewlinesAfter TRArrow        = True
> soaksNewlinesAfter TTriangle      = True
> soaksNewlinesAfter TTwoDot        = True
> soaksNewlinesAfter TCap           = True
> soaksNewlinesAfter TThreePipe     = True
> soaksNewlinesAfter (TRefinedBy _) = True
> soaksNewlinesAfter TAnd           = True
> soaksNewlinesAfter TElse          = True
> soaksNewlinesAfter TIf            = True
> soaksNewlinesAfter TLet           = True
> soaksNewlinesAfter TNot           = True
> soaksNewlinesAfter TOr            = True
> soaksNewlinesAfter TThen          = True
> soaksNewlinesAfter TWithin        = True
> soaksNewlinesAfter _              = False

> removeNewlinesAfterSoaker :: [Token] -> [Token]
> removeNewlinesAfterSoaker ts = fst $ foldr reducer ([], False) ts
>     where
>       reducer TNewline (ts, _)     = (ts, True)
>       reducer t        (ts, True)
>           | soaksNewlinesAfter t   = (t:ts, False)
>           | otherwise              = (t:TNewline:ts, False)
>       reducer t        (ts, False) = (t:ts, False)


String together all of the functions that make up the lexer.

> lexer
>     = removeNewlinesAfterSoaker .
>       removeNewlinesBeforeSoaker .
>       reduceNewlines .
>       baseLexer
