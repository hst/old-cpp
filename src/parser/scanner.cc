/*----------------------------------------------------------------------
 *
 *  Copyright © 2007, 2008 Douglas Creager
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

#ifndef HST_PARSER_SCANNER_CC
#define HST_PARSER_SCANNER_CC

#include <istream>
#include <cctype>

#include <hst/parser/scanner.hh>

#include "location.hh"

using namespace std;

namespace hst_parser
{
    static inline
    char read_char_skip_ws(istream &stream, location *loc)
    {
        char  ch;

        ch = stream.get();
        loc->columns(1);

        while (stream.good() && isspace(ch))
        {
            if (ch == '\n')
            {
                loc->lines(1);
            }
            loc->step();

            ch = stream.get();
            loc->columns(1);
        }

        return ch;
    }

    static inline
    char read_char(istream &stream, location *loc)
    {
        char  ch;

        ch = stream.get();
        if (ch == '\n')
        {
            loc->lines(1);
        } else {
            loc->columns(1);
        }

        return ch;
    }

    static inline
    char peek_char(istream &stream)
    {
        return stream.peek();
    }

#define RETURN_BAD_CHAR(msg)      \
    {                             \
        lval->str_val = msg;      \
        return token::BAD_CHAR;   \
    }

    token_type lexer::operator ()
        (semantic_type *lval, location *loc)
    {
        int  ch;

        loc->step();

        ch = read_char_skip_ws(stream, loc);

        // If we hit EOF before reading a real character, then we
        // return the END token.  Once we read a real character, we
        // *don't* return an END token, because at the very least, we
        // can return a BAD_CHAR token with the real characters we
        // read.

        if (ch == EOF) return token::END;

        // If we find any comments, go ahead and parse them.

        while (ch == '#')
        {
            // This comment ends at the end of the current line.
            do
            {
                ch = read_char(stream, loc);
            } while (ch != '\n');

            // Read the next character after the comment.  If it is
            // yet another comment, we'll loop back through here to
            // read it.

            ch = read_char_skip_ws(stream, loc);
        }

        // Let's get all the easy single-char tokens out of the way
        // first.

        if (ch == '{')
        {
            return token::LBRACE;
        } else if (ch == '}') {
            return token::RBRACE;
        } else if (ch == ',') {
            return token::COMMA;
        } else if (ch == '=') {
            return token::EQUALS;
        } else if (ch == ';') {
            return token::SEMI;
        } else if (ch == '\\') {
            return token::BACKSLASH;
        }

        // Tokens that start with "-"

        if (ch == '-')
        {
            int lookahead = peek_char(stream);

            if (lookahead == '>')
            {
                // found a "->"
                read_char(stream, loc);
                return token::ARROW;
            } else if (lookahead == '-') {
                read_char(stream, loc);
                lookahead = peek_char(stream);

                if (lookahead == '>')
                {
                    // found a "-->"
                    read_char(stream, loc);
                    return token::DASH_ARROW;
                }

                // found a bare "--"
                return token::DASH;
            }

            RETURN_BAD_CHAR(new string("-"));
        }

        // Tokens that start with "["

        if (ch == '[')
        {
            int lookahead = peek_char(stream);

            if (lookahead == ']')
            {
                // found a "[]"
                read_char(stream, loc);
                return token::BOX;
            } else if (lookahead == '|') {
                // found a "[|"
                read_char(stream, loc);
                return token::LPAR;
            } else if (lookahead == '[') {
                // found a "[["
                read_char(stream, loc);
                return token::LMAP;
            } else if (lookahead == '>') {
                // found a "[>"
                read_char(stream, loc);
                return token::RTRIANGLE;
            }

            // found a bare "["
            return token::LBRACKET;
        }

        // Tokens that start with "]"

        if (ch == ']')
        {
            int lookahead = peek_char(stream);

            if (lookahead == ']')
            {
                // found a "]]"
                read_char(stream, loc);
                return token::RMAP;
            }

            // found a bare "]"
            return token::RBRACKET;
        }

        // Tokens that start with "/"

        if (ch == '/')
        {
            int lookahead = peek_char(stream);

            if (lookahead == '\\')
            {
                // found a "/\"
                read_char(stream, loc);
                return token::TRIANGLE;
            }

            RETURN_BAD_CHAR(new string("/"));
        }

        // Tokens that start with "|"

        if (ch == '|')
        {
            int lookahead = peek_char(stream);

            if (lookahead == ']')
            {
                // found a "|]"
                read_char(stream, loc);
                return token::RPAR;
            } else if (lookahead == '|') {
                read_char(stream, loc);
                lookahead = peek_char(stream);

                if (lookahead == '|')
                {
                    // found a "|||"
                    read_char(stream, loc);
                    return token::THREE_PIPE;
                }

                // found a bare "||"
                return token::TWO_PIPE;

            } else if (lookahead == '~') {
                read_char(stream, loc);
                lookahead = peek_char(stream);

                if (lookahead == '|')
                {
                    // found a "|~|"
                    read_char(stream, loc);
                    return token::CAP;
                }

                RETURN_BAD_CHAR(new string("|~"));
            }

            RETURN_BAD_CHAR(new string("|"));
        }

        // Right, that's all of the operator tokens.  Next check for a
        // number.

        if (isdigit(ch))
        {
            unsigned long  val = (ch - '0');
            string  *msg = new string();
            *msg += ch;

            while (isdigit(peek_char(stream)))
            {
                unsigned long  old = val;
                ch = read_char(stream, loc);

                val *= 10;
                val += (ch - '0');
                *msg += ch;

                // If the value has decreased, then we've overflowed!
                if (old > val)
                {
                    RETURN_BAD_CHAR(msg);
                }
            }

            // We didn't overflow, so we throw away that string we
            // spent so much time building and return the actual
            // value.

            delete msg;
            lval->ul_val = val;
            return token::ULONG;
        }

        // Last thing it can be is an identifier.  Let's go!
        if (isalpha(ch) || ch == '_' || ch == '$')
        {
            string  *id = new string();
            *id += ch;

            if (ch == '$')
            {
                // If the ID starts with a $, then there has to be at
                // least one more character.

                int lookahead = peek_char(stream);

                if (isalpha(lookahead) || isdigit(lookahead) || ch == '_')
                {
                    read_char(stream, loc);
                    *id += lookahead;
                } else {
                    RETURN_BAD_CHAR(id);
                }
            }

            ch = peek_char(stream);
            while (isalpha(ch) || isdigit(ch) || ch == '_')
            {
                read_char(stream, loc);
                *id += ch;
                ch = peek_char(stream);
            }

            // Right, we've found an identifier.  Check to see if it's
            // one of the keywords; if so, return that keyword token
            // instead.  Otherwise return an identifier token.


            if (*id == "alias")
            {
                delete id;
                return token::ALIAS;
            } else if (*id == "aparallel") {
                delete id;
                return token::APARALLEL;
            } else if (*id == "event") {
                delete id;
                return token::EVENT;
            } else if (*id == "extchoice") {
                delete id;
                return token::EXTCHOICE;
            } else if (*id == "hide") {
                delete id;
                return token::HIDE;
            } else if (*id == "intchoice") {
                delete id;
                return token::INTCHOICE;
            } else if (*id == "interleave") {
                delete id;
                return token::INTERLEAVE;
            } else if (*id == "timeout") {
                delete id;
                return token::TIMEOUT;
            } else if (*id == "iparallel") {
                delete id;
                return token::IPARALLEL;
            } else if (*id == "prefix") {
                delete id;
                return token::PREFIX;
            } else if (*id == "process") {
                delete id;
                return token::PROCESS;
            } else if (*id == "rename") {
                delete id;
                return token::RENAME;
            } else if (*id == "rextchoice") {
                delete id;
                return token::REXTCHOICE;
            } else if (*id == "seqcomp") {
                delete id;
                return token::SEQCOMP;
            }

            lval->str_val = id;
            return token::ID;
        }

        // Nothing!  That's a parse error.
        RETURN_BAD_CHAR(new string(1, ch));
    }
}

#endif // HST_PARSER_SCANNER_CC
