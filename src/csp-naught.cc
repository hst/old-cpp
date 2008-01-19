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

#ifndef HST_CSP_NAUGHT_CC
#define HST_CSP_NAUGHT_CC

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/io.hh>
#include <hst/io-macros.hh>

using namespace std;

#define NOTHING (void) 0

namespace hst
{
    static const string  ID_START =
        "$_"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    static const string  ID_CONT =
        "_0123456789"
        "ABCDEFGHIJKLMNOPQRSTUVWXYZ"
        "abcdefghijklmnopqrstuvwxyz";

    static
    void read_identifier(istream &stream, string &str,
                         const bool skip_space)
    {
        string  result;
        char    ch;

#if HST_IO_DEBUG
        cerr << "Require identifier" << endl;
#endif

        // Try to read an initial identifier character, possibly
        // skipping over any preceding whitespace.  If the character
        // isn't an identifier character, it's a parse error.  If it
        // is, that's the start of the identifier.

        require_any_char(stream, ch, ID_START, skip_space);
        PROPAGATE_ANY_ERROR(NOTHING);
        result = ch;

        // Try to read further identifier characters.  If we ever
        // reach EOF, that marks the end of the value, so we can
        // return it and a success code.  If any other error condition
        // occurs, we return.  If we read a non-identifier character,
        // that marks the end of the identifier, so we push the
        // character back onto the stream and return the identifier.

        while (true)
        {
            require_any_char(stream, ch, ID_CONT, false);

            if (stream.eof())
            {
                // Keep the EOF flag set, but do *not* set the fail
                // flag, since we've successfully read an identifier.

#if HST_IO_DEBUG
                cerr << "  got \"" << result << "\"" << endl;
#endif
                stream.clear();
                stream.setstate(ios_base::eofbit);
                str = result;
                return;
            } else if (stream.bad()) {
                return;
            } else if (stream.fail()) {
                stream.clear();
                stream.putback(ch);
#if HST_IO_DEBUG
                cerr << "  got \"" << result << "\"" << endl;
#endif
                str = result;
                return;
            } else {
                result += ch;
            }
        }
    }

    static const string  KEYWORD =
        "abcdefghijklmnopqrstuvwxyz";

    static
    void read_keyword(istream &stream, string &str,
                      const bool skip_space)
    {
        string  result;
        char    ch;

#if HST_IO_DEBUG
        cerr << "Require keyword" << endl;
#endif

        // Try to read an initial keyword character, possibly skipping
        // over any preceding whitespace.  If the character isn't an
        // keyword character, it's a parse error.  If it is, that's
        // the start of the keyword.

        require_any_char(stream, ch, KEYWORD, skip_space);
        PROPAGATE_ANY_ERROR(NOTHING);
        result = ch;

        // Try to read further keyword characters.  If we ever reach
        // EOF, that marks the end of the value, so we can return it
        // and a success code.  If any other error condition occurs,
        // we return.  If we read a non-keyword character, that marks
        // the end of the keyword, so we push the character back onto
        // the stream and return the keyword.

        while (true)
        {
            require_any_char(stream, ch, KEYWORD, false);

            if (stream.eof())
            {
                // Keep the EOF flag set, but do *not* set the fail
                // flag, since we've successfully read an keyword.

#if HST_IO_DEBUG
                cerr << "  got \"" << result << "\"" << endl;
#endif
                stream.clear();
                stream.setstate(ios_base::eofbit);
                str = result;
                return;
            } else if (stream.bad()) {
                return;
            } else if (stream.fail()) {
                stream.clear();
                stream.putback(ch);
#if HST_IO_DEBUG
                cerr << "  got \"" << result << "\"" << endl;
#endif
                str = result;
                return;
            } else {
                result += ch;
            }
        }
    }

#define READ_IDENTIFIER(id)                      \
    {                                            \
        read_identifier(stream, (id), true);     \
        EOF_IS_ERROR;                            \
        PROPAGATE_ANY_ERROR(NOTHING);            \
    }

#define REQUIRE_CHAR(ch)                        \
    {                                           \
        require_char(stream, (ch), true);       \
        EOF_IS_ERROR;                           \
        PROPAGATE_ANY_ERROR(NOTHING);           \
    }

#define REQUIRE_STRING(str)                     \
    {                                           \
        require_string(stream, (str), true);    \
        EOF_IS_ERROR;                           \
        PROPAGATE_ANY_ERROR(NOTHING);           \
    }

    static
    void read_process_definition(istream &stream, csp_t &csp)
    {
        string  id;

        READ_IDENTIFIER(id);
        REQUIRE_CHAR(';');

        csp.add_process(id);
    }

    static
    void read_event_definition(istream &stream, csp_t &csp)
    {
        string  id;

        READ_IDENTIFIER(id);
        REQUIRE_CHAR(';');

        csp.add_event(id);
    }

    static
    void read_process(istream &stream, csp_t &csp,
                      state_t &state)
    {
        string  id;

        READ_IDENTIFIER(id);

        state = csp.get_process(id);
        if (state == HST_ERROR_STATE)
            PARSE_ERROR(NOTHING);
    }

#define READ_PROCESS(P)                         \
    {                                           \
        read_process(stream, csp, (P));         \
        EOF_IS_ERROR;                           \
        PROPAGATE_ANY_ERROR(NOTHING);           \
    }

    static
    void read_event(istream &stream, csp_t &csp,
                    event_t &event)
    {
        string  id;

        READ_IDENTIFIER(id);

        event = csp.get_event(id);
        if (event == HST_ERROR_EVENT)
            PARSE_ERROR(NOTHING);
    }

#define READ_EVENT(event)                       \
    {                                           \
        read_event(stream, csp, (event));       \
        EOF_IS_ERROR;                           \
        PROPAGATE_ANY_ERROR(NOTHING);           \
    }

    static
    void read_prefix(istream &stream, csp_t &csp)
    {
        state_t  dest;
        event_t  a;
        state_t  P;

        // prefix [dest] = [a] -> [P];
        // (Initial keyword will have been read already)

        READ_PROCESS(dest);
        REQUIRE_CHAR('=');
        READ_EVENT(a);
        REQUIRE_STRING("->")
        READ_PROCESS(P);
        REQUIRE_CHAR(';');

        csp.prefix(dest, a, P);
    }

    static
    void read_extchoice(istream &stream, csp_t &csp)
    {
        state_t  dest;
        state_t  P, Q;

        // extchoice [dest] = [P] [] [Q];
        // (Initial keyword will have been read already)

        READ_PROCESS(dest);
        REQUIRE_CHAR('=');
        READ_PROCESS(P);
        REQUIRE_STRING("[]")
        READ_PROCESS(Q);
        REQUIRE_CHAR(';');

        csp.extchoice(dest, P, Q);
    }

    static
    void read_intchoice(istream &stream, csp_t &csp)
    {
        state_t  dest;
        state_t  P, Q;

        // intchoice [dest] = [P] |~| [Q];
        // (Initial keyword will have been read already)

        READ_PROCESS(dest);
        REQUIRE_CHAR('=');
        READ_PROCESS(P);
        REQUIRE_STRING("|~|")
        READ_PROCESS(Q);
        REQUIRE_CHAR(';');

        csp.intchoice(dest, P, Q);
    }

    static
    void read_interrupt(istream &stream, csp_t &csp)
    {
        state_t  dest;
        state_t  P, Q;

        // interrupt [dest] = [P] /\ [Q];
        // (Initial keyword will have been read already)

        READ_PROCESS(dest);
        REQUIRE_CHAR('=');
        READ_PROCESS(P);
        REQUIRE_STRING("/\\")
        READ_PROCESS(Q);
        REQUIRE_CHAR(';');

        csp.interrupt(dest, P, Q);
    }

    static
    void read_seqcomp(istream &stream, csp_t &csp)
    {
        state_t  dest;
        state_t  P, Q;

        // seqcomp [dest] = [P] ; [Q];
        // (Initial keyword will have been read already)

        READ_PROCESS(dest);
        REQUIRE_CHAR('=');
        READ_PROCESS(P);
        REQUIRE_CHAR(';')
        READ_PROCESS(Q);
        REQUIRE_CHAR(';');

        csp.seqcomp(dest, P, Q);
    }

    static
    void read_statement(istream &stream, csp_t &csp)
    {
        string  keyword;

        // Read the statement name.
        read_keyword(stream, keyword, true);
        PROPAGATE_ANY_ERROR(NOTHING);

        if (keyword == "process")
            read_process_definition(stream, csp);
        else if (keyword == "event")
            read_event_definition(stream, csp);
        else if (keyword == "prefix")
            read_prefix(stream, csp);
        else if (keyword == "extchoice")
            read_extchoice(stream, csp);
        else if (keyword == "intchoice")
            read_intchoice(stream, csp);
        else if (keyword == "interrupt")
            read_interrupt(stream, csp);
        else if (keyword == "seqcomp")
            read_seqcomp(stream, csp);

        PROPAGATE_ANY_ERROR(NOTHING);
    }

    void read_csp0(istream &stream, csp_t &csp)
    {
        csp_t  result;

        while (true)
        {
            read_statement(stream, result);

            if (stream.eof())
            {
                /*
                 * If we've reached the end of the stream, then we
                 * successfully read the CSP description.
                 */

                stream.clear();
                stream.setstate(ios_base::eofbit);

                csp.swap(result);
                return;
            }

            /*
             * Return any other errors without updating the output
             * variable.
             */

            PROPAGATE_ANY_ERROR(NOTHING);

            /*
             * And if there were no errors, loop back and try to read
             * another statement.
             */
            stream.get();
        }
    }
}

#endif // HST_CSP_NAUGHT_CC
