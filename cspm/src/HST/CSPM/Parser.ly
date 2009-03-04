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
>   "_"          { TWildcard }
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
>   bool         { TBool }
>   card         { TCard }
>   channel      { TChannel }
>   chaos        { TChaos }
>   datatype     { TDatatype }
>   diff         { TDiff }
>   dinter       { TDInter }
>   dunion       { TDUnion }
>   elem         { TElem }
>   else         { TElse }
>   empty        { TEmpty }
>   external     { TExternal }
>   false        { TFalse }
>   head         { THead }
>   if           { TIf }
>   int          { TInt }
>   inter        { TInter }
>   length       { TLength }
>   let          { TLet }
>   member       { TMember }
>   nametype     { TNametype }
>   not          { TNot }
>   null         { TNull }
>   or           { TOr }
>   powerset     { TPowerset }
>   pragma       { TPragma }
>   print        { TPrint }
>   sequenceset  { TSequenceset }
>   set          { TSet }
>   skip         { TSkip }
>   stop         { TStop }
>   tail         { TTail }
>   then         { TThen }
>   transparent  { TTransparent }
>   true         { TTrue }
>   union        { TUnion }
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
> PDefinitions  : PDefinition                { $1 }
>               | PDefinitions PNewlines
>                 PDefinition                { $1 ++ $3 }

> PDefinition :: { [Definition] }
> PDefinition  : PPattern "=" PExpr          { [DPatternDefn $1 $3] }
>              | PId "(" PPatterns0 ")"
>                "=" PExpr                   { [DLambdaClause $1 $
>                                              Clause (PTuple $3) $6] }
>              | channel PIds                { map DSimpleChannel $2 }
>              | nametype PId "=" PType      { [DNametype $2 $4] }
>              | datatype PId "="
>                PConstructors               { [DDatatype $2 $4] }

> PConstructors :: { [DConstructor] }
> PConstructors  : PConstructor              { [$1] }
>                | PConstructors "|"
>                  PConstructor              { $1 ++ [$3] }

> PConstructor :: { DConstructor }
> PConstructor  : PId                        { DSimpleConstructor $1 }
>               | PId "." PType              { DDataConstructor $1 $3 }

> PPatterns :: { [Pattern] }
> PPatterns  : PPattern                      { [$1] }
>            | PPatterns "," PPattern        { $1 ++ [$3] }

> PPatterns0 :: { [Pattern] }
> PPatterns0  :                              { [] }
>             | PPatterns                    { $1 }

> PPattern :: { Pattern }
> PPattern  : number                         { PNLit $1 }
>           | "_"                            { PWildcard }
>           | PId                            { PIdentifier $1 }
>           | "(" PPattern ","
>             PPatterns0 ")"                 { PTuple ($2:$4) }
>           | PPattern "." PPattern          { PDot $1 $3 }
>           | "<" PPatterns0 ">"             { PQLit $2 }
>           | PPattern "^" PPattern          { PQConcat $1 $3 }
>           | "{" "}"                        { PSEmpty }
>           | "{" PPattern "}"               { PSSingleton $2 }
>           | PPattern "@" PPattern          { PConjunction $1 $3 }

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
>        | PDot_                             { $1 }
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
>           | length "(" PExpr ")"           { EQLength $3 }
>           | card "(" PExpr ")"             { ESCardinality $3 }

> PSequence_ :: { Expression }
> PSequence_  : "<" PQExprs0 ">"             { EQLit $2 }
>             | "<" PExpr ".." PExpr ">"     { EQClosedRange $2 $4 }
>             | "<" PExpr ".." ">"           { EQOpenRange $2 }
>             | PExpr "^" PExpr              { EQConcat $1 $3 }
>             | tail "(" PExpr ")"           { EQTail $3 }

> PSet_ :: { Expression }
> PSet_  : bool                              { ESBool }
>        | int                               { ESInt }
>        | "{" PExprs0 "}"                   { ESLit $2 }
>        | "{" PExpr ".." PExpr "}"          { ESClosedRange $2 $4 }
>        | "{" PExpr ".." "}"                { ESOpenRange $2 }
>        | union "(" PExpr "," PExpr ")"     { ESUnion $3 $5 }
>        | inter "(" PExpr "," PExpr ")"     { ESIntersection $3 $5 }
>        | diff "(" PExpr "," PExpr ")"      { ESDifference $3 $5 }
>        | dunion "(" PExpr ")"              { ESDistUnion $3 }
>        | dinter "(" PExpr ")"              { ESDistIntersection $3 }
>        | set "(" PExpr ")"                 { EQSet $3 }
>        | powerset "(" PExpr ")"            { ESPowerset $3 }
>        | sequenceset "(" PExpr ")"         { ESSequenceset $3 }

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
>            | null "(" PExpr ")"            { EQEmpty $3 }
>            | elem "(" PExpr "," PExpr ")"  { EQIn $3 $5 }
>            | empty "(" PExpr ")"           { ESEmpty $3 }
>            | member "(" PExpr ","
>              PExpr ")"                     { ESIn $3 $5 }

> PTuple_ :: { Expression }
> PTuple_  : "(" PExpr "," PExprs0 ")"       { ETLit ($2:$4) }

> PDot_ :: { Expression }
> PDot_  : PExpr "." PExpr                   { EDot $1 $3 }

> PLambda_ :: { Expression }
> PLambda_  : "\\" PIds "@" PExpr
>             { ELambda [Clause (PTuple $ map PIdentifier $2) $4] }

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
>         | "|~|" PExpr                      { ERIntChoice $2 }

> PAny_ :: { Expression }
> PAny_  : PId                               { EVar $1 }
>        | "(" PExpr ")"                     { $2 }
>        | let PSimpleDefns within PExpr     { ELet $2 $4 }
>        | PExpr "(" PExprs0 ")"             { EApply $1 $3 }
>        | head "(" PExpr ")"                { EQHead $3 }
>        | if PExpr then PExpr else PExpr    { EIfThenElse $2 $4 $6 }

> PType :: { Expression }
> PType  : bool                              { ESBool }
>        | int                               { ESInt }
>        | "{" PExprs0 "}"                   { ESLit $2 }
>        | "{" PExpr ".." PExpr "}"          { ESClosedRange $2 $4 }
>        | "{" PExpr ".." "}"                { ESOpenRange $2 }
>        | "(" PType "," PTypes ")"          { ESTupleProduct ($2:$4) }
>        | PType "." PType                   { ESDotProduct $1 $3 }
>        | union "(" PType "," PType ")"     { ESUnion $3 $5 }
>        | inter "(" PType "," PType ")"     { ESIntersection $3 $5 }
>        | diff "(" PType "," PType ")"      { ESDifference $3 $5 }
>        | dunion "(" PType ")"              { ESDistUnion $3 }
>        | dinter "(" PType ")"              { ESDistIntersection $3 }
>        | set "(" PType ")"                 { EQSet $3 }
>        | powerset "(" PType ")"            { ESPowerset $3 }
>        | sequenceset "(" PType ")"         { ESSequenceset $3 }
>        | PId                               { EVar $1 }
>        | PId "(" PTypes ")"                { EApply (EVar $1) $3 }

> PTypes :: { [Expression] }
> PTypes  : PType                            { [$1] }
>         | PTypes "," PType                 { $1 ++ [$3] }

> PSimpleDefns :: { [Definition] }
> PSimpleDefns  : PSimpleDefn                { [$1] }
>               | PSimpleDefns PNewlines
>                 PSimpleDefn                { $1 ++ [$3] }

> PSimpleDefn :: { Definition }
> PSimpleDefn  : PPattern "=" PExpr          { DPatternDefn $1 $3 }
>              | PId "(" PPatterns0 ")"
>                "=" PExpr                   { DLambdaClause $1 $
>                                              Clause (PTuple $3) $6 }

> {

> parseError :: [Token] -> a
> parseError _ = error ("Parse error\n")

> parseFile = parseFile_ . lexer
> parseExpr = parseExpr_ . lexer

> }