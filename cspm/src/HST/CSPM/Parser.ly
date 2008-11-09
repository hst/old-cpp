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

> {
> module HST.CSPM.Parser
>     (
>      parseFile, parseExpr
>     )
>     where
>
> import HST.CSPM.Types
> import HST.CSPM.Lexer
> }

> %name parseFile_ PRoot
> %name parseExpr_ PExpr
> %tokentype { Token }
> %error { parseError }

> %token
>   "("          { TLParen }
>   ")"          { TRParen }
>   "{"          { TLBrace }
>   "}"          { TRBrace }
>   "["          { TLBracket }
>   "]"          { TRBracket }
>   "<"          { TLAngle }
>   ">"          { TRAngle }
>   "\\"         { TBackslash }
>   "|"          { TPipe }
>   ":"          { TColon }
>   "&"          { TAmpersand }
>   ";"          { TSemi }
>   "."          { TDot }
>   "?"          { TQMark }
>   "!"          { TBang }
>   "@"          { TAt }
>   ","          { TComma }
>   "="          { TEquals }
>   "+"          { TPlus }
>   "-"          { TMinus }
>   "*"          { TAsterisk }
>   "/"          { TSlash }
>   "%"          { TPercent }
>   "#"          { THash }
>   "^"          { TCaret }
>   "[["         { TLMap }
>   "]]"         { TRMap }
>   "{|"         { TLPBrace }
>   "|}"         { TRPBrace }
>   "[|"         { TLPar }
>   "|]"         { TRPar }
>   "=="         { TEq }
>   "!="         { TNE }
>   "<="         { TLE }
>   ">="         { TGE }
>   "[>"         { TRTriangle }
>   "[]"         { TBox }
>   "||"         { TTwoPipe }
>   "<-"         { TLArrow }
>   "->"         { TRArrow }
>   "/\\"        { TTriangle }
>   ".."         { TTwoDot }
>   "|~|"        { TCap }
>   "|||"        { TThreePipe }
>   "[="         { TRefinedBy $$ }
>   and          { TAnd }
>   assert       { TAssert }
>   channel      { TChannel }
>   chaos        { TChaos }
>   datatype     { TDatatype }
>   else         { TElse }
>   external     { TExternal }
>   false        { TFalse }
>   if           { TIf }
>   let          { TLet }
>   not          { TNot }
>   or           { TOr }
>   pragma       { TPragma }
>   print        { TPrint }
>   skip         { TSkip }
>   stop         { TStop }
>   then         { TThen }
>   transparent  { TTransparent }
>   true         { TTrue }
>   within       { TWithin }
>   identifier   { TIdentifier $$ }
>   number       { TNumber $$ }
>   nl           { TNewline }
>   badchar      { TBadChar }

> %nonassoc let within if then else
> %nonassoc ":" "@"
> %left     "\\"
> %left     "|||"
> %nonassoc "[|" "|]" "[" "||" "]"
> %left     "|~|"
> %left     "[]"
> %left     "/\\"
> %left     "[>"
> %left     ";"
> %nonassoc "&"
> %right    "->" "?" "!"
> %right    "."
> %left     or
> %left     and
> %left     not
> %nonassoc "==" "!=" "<" ">" "<=" ">="
> %nonassoc ENDSEQ
> %left     "+" "-"
> %left     "*" "/" "%"
> %left     "#"
> %left     "^"
> %nonassoc "(" ")" "[[" "]]"

> %%

> PRoot :: { CSPMScript }
> PRoot  : PNewlines0 PDefinitions
>          PNewlines0                        { CSPMScript $2 }

> PNewlines :: { () }
> PNewlines  : nl PNewlines0                 { () }

> PNewlines0 :: { () }
> PNewlines0  :                              { () }
>             | PNewlines                    { () }

> PDefinitions :: { [Definition] }
> PDefinitions  : PDefinition                { [$1] }
>               | PDefinitions PNewlines
>                 PDefinition                { $1 ++ [$3] }

> PDefinition :: { Definition }
> PDefinition  : PId "=" PExpr               { DDefinition $1 $3 }
>              | channel PId                 { DSimpleChannel $2 }

> PId :: { Identifier }
> PId  : identifier                          { Identifier $1 }

> PIds :: { [Identifier] }
> PIds  : PId                                { [$1] }
>       | PIds "," PId                       { $1 ++ [$3] }

> PExprs :: { [Expression] }
> PExprs  : PExpr                            { [$1] }
>         | PExprs "," PExpr                 { $1 ++ [$3] }

> PExprs0 :: { [Expression] }
> PExprs0  :                                 { [] }
>          | PExprs                          { $1 }

> PQExprs :: { [Expression] }
> PQExprs  : PExpr %prec ENDSEQ              { [$1] }
>          | PQExprs "," PExpr %prec ENDSEQ  { $1 ++ [$3] }

> PQExprs0 :: { [Expression] }
> PQExprs0  :                                { [] }
>           | PQExprs                        { $1 }

> PExpr :: { Expression }
> PExpr  : PNumber_                          { $1 }
>        | PSequence_                        { $1 }
>        | PSet_                             { $1 }
>        | PBoolean_                         { $1 }
>        | PTuple_                           { $1 }
>        | PLambda_                          { $1 }
>        | PProc_                            { $1 }
>        | PAny_                             { $1 }

> PNumber_ :: { Expression }
> PNumber_  : number                         { ENLit $1 }
>           | "-" PExpr                      { ENNeg $2 }
>           | PExpr "+" PExpr                { ENSum $1 $3 }
>           | PExpr "-" PExpr                { ENDiff $1 $3 }
>           | PExpr "*" PExpr                { ENProd $1 $3 }
>           | PExpr "/" PExpr                { ENQuot $1 $3 }
>           | PExpr "%" PExpr                { ENRem $1 $3 }
>           | "#" PExpr                      { EQLength $2 }

> PSequence_ :: { Expression }
> PSequence_  : "<" PQExprs0 ">"             { EQLit $2 }
>             | "<" PExpr ".." PExpr ">"     { EQClosedRange $2 $4 }
>             | "<" PExpr ".." ">"           { EQOpenRange $2 }
>             | PExpr "^" PExpr              { EQConcat $1 $3 }

> PSet_ :: { Expression }
> PSet_  : "{" PExprs0 "}"                   { ESLit $2 }
>        | "{" PExpr ".." PExpr "}"          { ESClosedRange $2 $4 }
>        | "{" PExpr ".." "}"                { ESOpenRange $2 }

> PBoolean_ :: { Expression }
> PBoolean_  : true                          { EBTrue }
>            | false                         { EBFalse }
>            | PExpr and PExpr               { EBAnd $1 $3 }
>            | PExpr or PExpr                { EBOr $1 $3 }
>            | not PExpr                     { EBNot $2 }
>            | PExpr "==" PExpr              { EEqual $1 $3 }
>            | PExpr "!=" PExpr              { ENotEqual $1 $3 }
>            | PExpr "<" PExpr               { ELT $1 $3 }
>            | PExpr ">" PExpr               { EGT $1 $3 }
>            | PExpr "<=" PExpr              { ELTE $1 $3 }
>            | PExpr ">=" PExpr              { EGTE $1 $3 }

> PTuple_ :: { Expression }
> PTuple_  : "(" PExpr "," PExprs0 ")"       { ETLit ($2:$4) }

> PLambda_ :: { Expression }
> PLambda_  : "\\" PIds "@" PExpr            { ELambda $2 $4 }

> PProc_ :: { Expression }
> PProc_  : stop                             { EStop }
>         | skip                             { ESkip }
>         | PExpr "->" PExpr                 { EPrefix $1 $3 }
>         | PExpr "[]" PExpr                 { EExtChoice $1 $3 }
>         | PExpr "|~|" PExpr                { EIntChoice $1 $3 }
>         | PExpr "[>" PExpr                 { ETimeout $1 $3 }
>         | PExpr ";" PExpr                  { ESeqComp $1 $3 }
>         | PExpr "|||" PExpr                { EInterleave $1 $3 }
>         | PExpr "[|" PExpr "|]" PExpr      { EIParallel $1 $3 $5 }
>         | PExpr "[" PExpr "||"
>           PExpr "]" PExpr                  { EAParallel $1 $3 $5 $7 }
>         | PExpr "\\" PExpr                 { EHide $1 $3 }
>         | "[]" PExpr                       { ERExtChoice $2 }

> PAny_ :: { Expression }
> PAny_  : PId                               { EVar $1 }
>        | "(" PExpr ")"                     { $2 }
>        | let PBindings within PExpr        { ELet $2 $4 }
>        | PExpr "(" PExprs0 ")"             { EApply $1 $3 }
>        | if PExpr then PExpr else PExpr    { EIfThenElse $2 $4 $6 }

> PBindings :: { [Binding] }
> PBindings  : PBinding                      { [$1] }
>            | PBindings PNewlines PBinding  { $1 ++ [$3] }

> PBinding :: { Binding }
> PBinding  : PId "=" PExpr                  { Binding $1 $3 }

> {

> parseError :: [Token] -> a
> parseError _ = error ("Parse error\n")

> parseFile = parseFile_ . lexer
> parseExpr = parseExpr_ . lexer

> }