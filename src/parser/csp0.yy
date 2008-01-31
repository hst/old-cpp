/* -*- C++ -*- */

%{ /*** C/C++ Declarations in header file ***/
#include <istream>
#include <string>

#include "location.hh"

#include <hst/csp.hh>
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
%start csp

/* write out a header file containing the token defines */
%defines

/* use newer C++ skeleton file */
%skeleton "lalr1.cc"

/* namespace to enclose parser in */
%name-prefix="hst_parser"

/* set the parser's class identifier */
%define "parser_class_name" "CSP0Parser"

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
%parse-param { csp_t &_result }

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
    string         *str_val;
    bool           dummy;
    alphabet_t     *alpha_val;
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

%type  <str_val>  new_process_id new_event_id
%printer { debug_stream() << *$$; }  new_process_id new_event_id
%destructor { delete $$; }  new_process_id new_event_id

%type  <ul_val>  process_id event_id
%printer { debug_stream() << $$; }  process_id event_id

%type  <alpha_val>  alphabet event_list
%printer { debug_stream() << *$$; }  alphabet event_list
%destructor { delete $$; }  alphabet event_list

%type  <dummy>       csp stmt
%type  <dummy>       process_def event_def prefix_stmt extchoice_stmt
%type  <dummy>       intchoice_stmt interrupt_stmt seqcomp_stmt
%type  <dummy>       interleave_stmt iparallel_stmt aparallel_stmt
%type  <dummy>       hide_stmt

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

csp
    : stmt
    { $$ = false; }
    | csp stmt
    { $$ = $2; }
    ;

stmt
    : process_def
    { $$ = $1; }
    | event_def
    { $$ = $1; }
    | prefix_stmt
    { $$ = $1; }
    | extchoice_stmt
    { $$ = $1; }
    | intchoice_stmt
    { $$ = $1; }
    | interrupt_stmt
    { $$ = $1; }
    | seqcomp_stmt
    { $$ = $1; }
    | interleave_stmt
    { $$ = $1; }
    | iparallel_stmt
    { $$ = $1; }
    | aparallel_stmt
    { $$ = $1; }
    | hide_stmt
    { $$ = $1; }
    ;

new_process_id
    : ID
    {
        state_t  process_id;
        process_id = _result.get_process(*$1);
        if (process_id != HST_ERROR_STATE)
        {
            // This process already exists!
            error(@1, "Process " + *$1 + " already exists");
            YYERROR;
        }

        $$ = $1;
    }
    ;

process_id
    : ID
    {
        state_t  process_id;
        process_id = _result.get_process(*$1);
        if (process_id == HST_ERROR_STATE)
        {
            // This process doesn't exist!
            error(@1, "Process " + *$1 + " doesn't exist");
            YYERROR;
        }

        delete $1;
        $$ = process_id;
    }
    ;

new_event_id
    : ID
    {
        state_t  event_id;
        event_id = _result.get_event(*$1);
        if (event_id != HST_ERROR_EVENT)
        {
            // This event already exists!
            error(@1, "Event " + *$1 + " already exists");
            YYERROR;
        }

        $$ = $1;
    }
    ;

event_id
    : ID
    {
        state_t  event_id;
        event_id = _result.get_event(*$1);
        if (event_id == HST_ERROR_EVENT)
        {
            // This event doesn't exist!
            error(@1, "Event " + *$1 + " doesn't exist");
            YYERROR;
        }

        delete $1;
        $$ = event_id;
    }
    ;

alphabet
    : LBRACE RBRACE
    { $$ = new alphabet_t(); }
    | LBRACE event_list RBRACE
    { $$ = $2; }
    ;

event_list
    : event_id
    {
        $$ = new alphabet_t();
        *$$ += $1;
    }
    | event_list COMMA event_id
    {
        $$ = $1;
        *$$ += $3;
    }
    ;

process_def
    : PROCESS new_process_id SEMI
    {
        _result.add_process(*$2);
        delete $2;
        $$ = false;
    }
    ;

event_def
    : EVENT new_event_id SEMI
    {
        _result.add_event(*$2);
        delete $2;
        $$ = false;
    }
    ;

prefix_stmt
    : PREFIX process_id EQUALS
      event_id ARROW process_id SEMI
    {
        _result.prefix($2, $4, $6);
    }
    ;

extchoice_stmt
    : EXTCHOICE process_id EQUALS
      process_id BOX process_id SEMI
    {
        _result.extchoice($2, $4, $6);
    }
    ;

intchoice_stmt
    : INTCHOICE process_id EQUALS
      process_id CAP process_id SEMI
    {
        _result.intchoice($2, $4, $6);
    }
    ;

interrupt_stmt
    : INTERRUPT process_id EQUALS
      process_id TRIANGLE process_id SEMI
    {
        _result.interrupt($2, $4, $6);
    }
    ;

seqcomp_stmt
    : SEQCOMP process_id EQUALS
      process_id SEMI process_id SEMI
    {
        _result.seqcomp($2, $4, $6);
    }
    ;

interleave_stmt
    : INTERLEAVE process_id EQUALS
      process_id THREE_PIPE process_id SEMI
    {
        _result.interleave($2, $4, $6);
    }
    ;

iparallel_stmt
    : IPARALLEL process_id EQUALS
      process_id LPAR alphabet RPAR process_id SEMI
    {
        _result.interface_parallel($2, $4, *$6, $8);
        delete $6;
    }
    ;

aparallel_stmt
    : APARALLEL process_id EQUALS
      process_id LBRACKET alphabet TWO_PIPE
      alphabet RBRACKET process_id SEMI
    {
        _result.alphabetized_parallel($2, $4, *$6, *$8, $10);
        delete $6;
        delete $8;
    }
    ;

hide_stmt
    : HIDE process_id EQUALS
      process_id BACKSLASH alphabet SEMI
    {
        _result.hide($2, $4, *$6);
        delete $6;
    }
    ;

%%

void hst_parser::CSP0Parser::error(const location &loc,
                                   const string &msg)
{
    cerr << loc << ": " << msg << endl;
}

namespace hst
{
    void read_csp0(istream &stream, csp_t &csp)
    {
        static string  filename("input file");

        csp_t  result;
        lexer  _lexer(stream);
        CSP0Parser  parser(_lexer, filename, result);

        //parser.set_debug_level(1);

        if (parser.parse() == 0)
        {
            stream.clear();
            stream.setstate(ios_base::eofbit);
            csp.swap(result);
            return;
        } else {
            stream.setstate(ios_base::failbit);
            return;
        }
    }
}
