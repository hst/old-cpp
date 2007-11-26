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

#ifndef HST_IO_HH
#define HST_IO_HH

#include <iostream>

using namespace std;

namespace hst
{
    /*
     * Works exactly like fgetc(), except that it skips over any
     * initial whitespace characters if «skip_space» is true.
     */

    void read_char_skip_space(istream &stream, char &ch,
                              const bool skip_space);

    /*
     * Requires that the next character in the input stream be
     * «expected».  Sets the stream's fail flag otherwise.
     */

    void require_char(istream &stream, const char expected,
                      const bool skip_space);

    /*
     * Requires that the next token in the input stream be the «str»
     * string.  Sets the stream's fail flag otherwise.
     */

    void require_string(istream &stream, const char *str,
                        const bool skip_space);

    /*
     * Reads a positive decimal integer literal from the input stream.
     * The literal must match the regexp "[0-9]+".  If we find a valid
     * integer literal, its value is placed into the «value»
     * reference.  Sets the stream's fail flag otherwise.
     */

    void read_word(istream &stream, unsigned long &value,
                   const bool skip_space);

}

#endif // HST_IO_HH
