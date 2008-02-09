/* -*- C++ -*- */

%{ /*** C/C++ Declarations in header file ***/
#include <istream>
#include <string>

#include "location.hh"

#include <hst/eventmap.hh>
#include <hst/parser/scanner.hh>

using namespace std;
using namespace hst;
using namespace hst_parser;
%}

/*** yacc/bison Declarations ***/

/* Require bison 2.3 or later */
%require "2.3"

/*
 * add debug output code to generated parser. disable this for release
 * versions.
 */
%debug

/* start symbol is named "intset" */
%start eventmap

/* write out a header file containing the token defines */
%defines

/* use newer C++ skeleton file */
%skeleton "lalr1.cc"

/* namespace to enclose parser in */
%name-prefix="hst_parser"

/* set the parser's class identifier */
%define "parser_class_name" "EventmapParser"

/* keep track of the current position within the input */
%locations
%initial-action
{
    // initialize the initial location object
    @$.begin.filename = @$.end.filename = &_filename;
};

/*
 * The driver is passed by reference to the parser and to the
 * scanner. This provides a simple but effective pure interface, not
 * relying on global variables.
 */
%parse-param { lexer &_lexer }
%parse-param { string &_filename }
%parse-param { eventmap_t &_result }

/* verbose error messages */
%error-verbose

/*
 * This union should start with the same elements as
 * hst_parser::semantic_type.  It can contain any other elements
 * needed by this grammar.
 */

%union
{
    unsigned long  ul_val;
    const string   *str_val;
    bool           dummy;
    eventpair_t    *pair_val;
}

%token END 0 "end of file"

%token <str_val>  BAD_CHAR  1
%token <str_val>  IO_ERROR  2

%printer { debug_stream() << *$$; } BAD_CHAR IO_ERROR
%destructor { delete $$; } BAD_CHAR IO_ERROR

/***** MAKE SURE THESE TOKENS MATCH THE ONES IN scanner.hh! *****/

/* operators */
%token LBRACE     300 "{"
%token RBRACE     301 "}"
%token LBRACKET   302 "["
%token RBRACKET   303 "]"
%token COMMA      304 ","
%token EQUALS     305 "="
%token SEMI       306 ";"
%token DASH       307 "--"
%token ARROW      308 "->"
%token DASH_ARROW 309 "-->"
%token BOX        310 "[]"
%token CAP        311 "|~|"
%token TRIANGLE   312 "/\\"
%token TWO_PIPE   313 "||"
%token THREE_PIPE 314 "|||"
%token LPAR       315 "[|"
%token RPAR       316 "|]"
%token BACKSLASH  317 "\\"
%token LMAP       318 "[["
%token RMAP       319 "]]"

/* keywords */
%token ALIAS      400 "alias"
%token APARALLEL  401 "aparallel"
%token EVENT      402 "event"
%token EXTCHOICE  403 "extchoice"
%token HIDE       404 "hide"
%token INTCHOICE  405 "intchoice"
%token INTERLEAVE 406 "interleave"
%token INTERRUPT  407 "interrupt"
%token IPARALLEL  408 "iparallel"
%token PREFIX     409 "prefix"
%token PROCESS    410 "process"
%token RENAME     411 "rename"
%token SEQCOMP    412 "seqcomp"

/* literals */
%token <ul_val>   ULONG 500 "number"
%token <str_val>  ID    501 "identifier"

%printer { debug_stream() << $$; }   ULONG
%printer { debug_stream() << *$$; }  ID
%destructor { delete $$; }  ID

%type  <dummy>       eventmap pair_list
%type  <pair_val>    pair

%printer { debug_stream() << $$->first << "->" << $$->second; }  pair
%destructor { delete $$; }  pair

%{ /*** C/C++ Declarations in code file ***/
#include <hst/parser/scanner.hh>

/*
 * this "connects" the bison parser in the driver to the flex scanner
 * class object. it defines the yylex() function call to pull the next
 * token from the current lexer object of the driver context.
 *
 * Since we are using a custom semantic_type definition, we have to
 * create a macro that calls the lexer with the right cast.
 */

#define cast_lexer(lval, loc) \
    _lexer((hst_parser::semantic_type *) lval, loc)

#undef yylex
#define yylex cast_lexer

%}

%%

eventmap
    : LBRACE RBRACE
    { $$ = false; YYACCEPT; }
    | LBRACE pair_list RBRACE
    { $$ = $2; YYACCEPT; }
    ;

pair_list
    : pair
      {
          _result.insert(*$1);
          delete $1;
          $$ = false;
      }
    | pair_list COMMA pair
      {
          _result.insert(*$3);
          delete $3;
          $$ = $1;
      }
    ;

pair
    : ULONG ARROW ULONG
    {
        $$ = new eventpair_t($1, $3);
    }
    ;

%%

void hst_parser::EventmapParser::error(const location &loc,
                                       const string &msg)
{
    cerr << loc << ": " << msg << endl;
}

namespace hst
{
    istream &operator >> (istream &stream, eventmap_t &eventmap)
    {
        static string  filename("input file");

        eventmap_t  result;
        lexer  _lexer(stream);
        EventmapParser  parser(_lexer, filename, result);

        //parser.set_debug_level(1);

        if (parser.parse() == 0)
        {
            eventmap.swap(result);
            return stream;
        } else {
            stream.setstate(ios_base::failbit);
            return stream;
        }
    }
}
