/*----------------------------------------------------------------------
 *
 *  Copyright (C) 2007 Douglas Creager
 *
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation; either
 *    version 2.1 of the License, or (at your option) any later
 *    version.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *    GNU Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the Free
 *    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
 *    MA 02111-1307 USA
 *
 *----------------------------------------------------------------------
 */

#ifndef HST_PARSER_SCANNER_HH
#define HST_PARSER_SCANNER_HH

#include <istream>
#include <string>

#include "location.hh"

using namespace std;

namespace hst_parser
{
    struct token
    {
        enum tokentype
        {
            END        = 0,
            BAD_CHAR   = 1,
            IO_ERROR   = 2,

            // operators
            LBRACE     = 300,
            RBRACE     = 301,
            LBRACKET   = 302,
            RBRACKET   = 303,
            COMMA      = 304,
            EQUALS     = 305,
            SEMI       = 306,
            DASH       = 307,
            ARROW      = 308,
            DASH_ARROW = 309,
            BOX        = 310,
            CAP        = 311,
            TRIANGLE   = 312,
            TWO_PIPE   = 313,
            THREE_PIPE = 314,
            LPAR       = 315,
            RPAR       = 316,
            BACKSLASH  = 317,

            // keywords
            APARALLEL  = 400,
            EVENT      = 401,
            EXTCHOICE  = 402,
            HIDE       = 403,
            INTCHOICE  = 404,
            INTERLEAVE = 405,
            INTERRUPT  = 406,
            IPARALLEL  = 407,
            PREFIX     = 408,
            PROCESS    = 409,
            SEQCOMP    = 410,

            // literals
            ULONG      = 500,
            ID         = 501
        };
    };

    typedef token::tokentype  token_type;

    union semantic_type
    {
        unsigned long  ul_val;
        const string   *str_val;
        bool           dummy;
    };

    struct lexer
    {
        istream  &stream;

        lexer(istream &_stream): stream(_stream) {}

        token_type operator () (semantic_type *lval, location *loc);
    };
}

#endif // HST_PARSER_SCANNER_HH
