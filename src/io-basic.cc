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

#include <exception>
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
        while (skip_space && stream.good() && isspace(ch))
        {
            ch = stream.get();
        }
    }

    void require_char(istream &stream, const char expected,
                      const bool skip_space)
    {
        char  ch;

        // Read the next character.  If it's the one we expected,
        // return successfully.  If it's any other character, it's a
        // parse error.

        read_char_skip_space(stream, ch, skip_space);
        PROPAGATE_ANY_ERROR(NOTHING);

        if (stream.good() && ch != expected)
        {
            stream.putback(ch);
            PARSE_ERROR(NOTHING);
        }
    }

    void require_string(istream &stream, const char *str,
                        const bool skip_space)
    {
        const char  *start;
        bool        first = true;

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

        return;

      error:
        // If we encounter a parse error part-way through the string,
        // there could be some initial characters that we've already
        // matched correctly.  If so, we should put them back into the
        // stream before returning.

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

        // Read the first character, possibly skipping over any
        // preceding whitespace.  If there's an error condition,
        // return.  If the character isn't a digit, it's a parse
        // error.  If it is a digit, that's the starting value for the
        // word we're reading.

        read_char_skip_space(stream, ch, skip_space);
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
                // Keep the EOF flag set, but do *not* set the fail
                // flag, since we've successfully read a value.

                value = result;
                return;
            } else if (isdigit(ch)) {
                result *= 10;
                result += (ch - '0');
            } else {
                stream.putback(ch);
                value = result;
                return;
            }
        } 
    }

} // namespace hst

#endif // HST_IO_BASIC_CC
