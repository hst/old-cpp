/* -*- C++ -*- */

%{ /*** C/C++ Declarations in header file ***/
#include <istream>
#include <string>

#include "location.hh"

#include <hst/lts.hh>
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
%start lts

/* write out a header file containing the token defines */
%defines

/* use newer C++ skeleton file */
%skeleton "lalr1.cc"

/* namespace to enclose parser in */
%name-prefix="hst_parser"

/* set the parser's class identifier */
%define "parser_class_name" "LTSParser"

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
%parse-param { lts_t &_result }

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
    lts_edge_t     *edge_val;
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

/* keywords */
%token APARALLEL  400 "aparallel"
%token EVENT      401 "event"
%token EXTCHOICE  402 "extchoice"
%token HIDE       403 "hide"
%token INTCHOICE  404 "intchoice"
%token INTERLEAVE 405 "interleave"
%token INTERRUPT  406 "interrupt"
%token IPARALLEL  407 "iparallel"
%token PREFIX     408 "prefix"
%token PROCESS    409 "process"
%token SEQCOMP    410 "seqcomp"

/* literals */
%token <ul_val>   ULONG 500 "number"
%token <str_val>  ID    501 "identifier"

%printer { debug_stream() << $$; }   ULONG
%printer { debug_stream() << *$$; }  ID
%destructor { delete $$; }  ID

%type  <dummy>       lts edge_list
%type  <edge_val>    edge

%printer
{
    debug_stream() << $$->from
                   << " --" << $$->event << "-> "
                   << $$->to;
}  edge
%destructor { delete $$; }  edge

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

lts
    : LBRACE RBRACE
    { $$ = false; YYACCEPT; }
    | LBRACE edge_list RBRACE
    { $$ = $2; YYACCEPT; }
    ;

edge_list
    : edge
      {
          _result.add_edge(*$1);
          delete $1;
          $$ = false;
      }
    | edge_list COMMA edge
      {
          _result.add_edge(*$3);
          delete $3;
          $$ = $1;
      }
    ;

edge
    : ULONG DASH ULONG DASH_ARROW ULONG
    {
        $$ = new lts_edge_t($1, $3, $5);
    }
    ;

%%

void hst_parser::LTSParser::error(const location &loc,
                                  const string &msg)
{
    cerr << loc << ": " << msg << endl;
}

namespace hst
{
    istream &operator >> (istream &stream, lts_t &lts)
    {
        static string  filename("input file");

        lts_t  result;
        lexer  _lexer(stream);
        LTSParser  parser(_lexer, filename, result);

        //parser.set_debug_level(1);

        if (parser.parse() == 0)
        {
            lts.swap(result);
            return stream;
        } else {
            stream.setstate(ios_base::failbit);
            return stream;
        }
    }
}
