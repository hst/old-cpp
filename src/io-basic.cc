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

#ifndef HST_IO_BASIC_CC
#define HST_IO_BASIC_CC

#include <iostream>
#include <cctype>

#include <hst/io.hh>
#include <hst/io-macros.hh>

using namespace std;

#define NOTHING (void) 0

namespace hst
{
    void read_char_skip_space(istream &stream, char &ch,
                              const bool skip_space)
    {
        ch = stream.get();

#if HST_IO_DEBUG
        cerr << "  read '" << ch << "'";
#endif

        while (skip_space && stream.good() && isspace(ch))
        {
            ch = stream.get();

#if HST_IO_DEBUG
            cerr << ", '" << ch << "'";
#endif
        }

#if HST_IO_DEBUG
        cerr << endl;
#endif
    }

    void require_char(istream &stream, const char expected,
                      const bool skip_space)
    {
        char  ch;

        // Read the next character.  If it's the one we expected,
        // return successfully.  If it's any other character, it's a
        // parse error.

#if HST_IO_DEBUG
        cerr << "Require '" << expected << "'" << endl;
#endif

        read_char_skip_space(stream, ch, skip_space);
        PROPAGATE_ANY_ERROR(NOTHING);

        if (stream.good() && ch != expected)
        {
#if HST_IO_DEBUG
            cerr << "  unsuccessful." << endl;
#endif

            stream.putback(ch);
            PARSE_ERROR(NOTHING);
        }

#if HST_IO_DEBUG
        cerr << "  successful." << endl;
#endif
    }

    void require_string(istream &stream, const char *str,
                        const bool skip_space)
    {
        const char  *start;
        bool        first = true;

#if HST_IO_DEBUG
        cerr << "Require \"" << str << "\"" << endl;
#endif

        while (*str != '\0')
        {
            // For the first character of the string, it's okay if we
            // reach EOF; we just report that to the caller.  If we
            // reach EOF in the *middle* of the string, though, that's
            // a parse error.

            if (first)
            {
                require_char(stream, *str, skip_space);
                first = false;
            } else {
                require_char(stream, *str, false);
                EOF_IS_ERROR;
            }
            FINALIZE_ANY_ERROR;

            str++;
        }

#if HST_IO_DEBUG
        cerr << "  successful." << endl;
#endif

        return;

      error:
        // If we encounter a parse error part-way through the string,
        // there could be some initial characters that we've already
        // matched correctly.  If so, we should put them back into the
        // stream before returning.

#if HST_IO_DEBUG
        cerr << "  unsuccessful." << endl;
#endif

        if (stream.fail())
        {
            // The character currently pointed to by [str] will have
            // been rejected by the most recent call to
            // require_char(), and thus will have already been pushed
            // onto the stream.  Therefore, we need to decrement the
            // pointer initially.

            while ((--str) >= start)
            {
                stream.putback(*str);
            }
        }

        return;
    }

    void read_word(istream &stream, unsigned long &value,
                   const bool skip_space)
    {
        unsigned long  result = 0;
        char           ch;

#if HST_IO_DEBUG
        cerr << "Require integer" << endl;
#endif

        // Read the first character, possibly skipping over any
        // preceding whitespace.  If there's an error condition,
        // return.  If the character isn't a digit, it's a parse
        // error.  If it is a digit, that's the starting value for the
        // word we're reading.

        read_char_skip_space(stream, ch, skip_space);
#if HST_IO_DEBUG
        cerr << "  ch = '" << ch << "'" << endl;
#endif
        PROPAGATE_IO_ERROR(NOTHING);

        if (isdigit(ch))
        {
            result = (ch - '0');
        } else {
            PARSE_ERROR(NOTHING);
        }

        // Try to read further digit characters, adding them into the
        // result as they're read.  If we ever reach EOF, that marks
        // the end of the value, so we can return it and a success
        // code.  If any other error condition occurs, we return.  If
        // we read a non-digit character, that marks the end of the
        // value, so we push the character back onto the stream and
        // return the parsed value.

        while (true)
        {
            read_char_skip_space(stream, ch, false);
            PROPAGATE_BAD(NOTHING);

            if (stream.eof())
            {
#if HST_IO_DEBUG
                cerr << "  got " << result << endl;
#endif
                // Keep the EOF flag set, but do *not* set the fail
                // flag, since we've successfully read a value.

                value = result;
                return;
            } else if (isdigit(ch)) {
                result *= 10;
                result += (ch - '0');
            } else {
#if HST_IO_DEBUG
                cerr << "  got " << result << endl;
#endif
                stream.putback(ch);
                value = result;
                return;
            }
        } 
    }

    void read_event_arrow(istream &stream, unsigned long &value,
                          const bool skip_space)
    {
        unsigned long  result = 0;

        /*
         * The first two characters must be '-'...
         */

        require_string(stream, "--", skip_space);
        PROPAGATE_ANY_ERROR(NOTHING);

        /*
         * ...followed by an integer literal (with no intervening
         * spaces)...
         */

        read_word(stream, result, false);
        EOF_IS_ERROR;
        PROPAGATE_ANY_ERROR(NOTHING);

        /*
         * ...followed by "-->"
         */

        require_string(stream, "-->", false);
        EOF_IS_ERROR;
        PROPAGATE_ANY_ERROR(NOTHING);

        /*
         * The parse succeeded, so let's return it.
         */

        value = result;
        return;
    }

    void read_lts_link(istream &stream,
                       unsigned long &from_state,
                       unsigned long &event,
                       unsigned long &to_state,
                       const bool skip_space)
    {
        unsigned long  from_result, to_result;
        unsigned long  event_result;

        /*
         * An LTS link should start with an integer...
         */

        read_word(stream, from_result, skip_space);
        PROPAGATE_ANY_ERROR(NOTHING);

        /*
         * ...followed by an event arrow...
         */

        read_event_arrow(stream, event_result, true);
        EOF_IS_ERROR;
        PROPAGATE_ANY_ERROR(NOTHING);

        /*
         * ...and then another integer.
         */
        read_word(stream, to_result, true);
        EOF_IS_ERROR;
        PROPAGATE_ANY_ERROR(NOTHING);

        /*
         * Right, we've parsed a valid LTS link, so let's return it.
         */

        from_state = from_result;
        event = event_result;
        to_state = to_result;
        return;
    }

} // namespace hst

#endif // HST_IO_BASIC_CC
