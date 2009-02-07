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

#ifndef ZOBRIST_CC
#define ZOBRIST_CC

#include <iostream>

#include <hst/zobrist.hh>

using namespace std;

namespace hst
{
    void zobrist_t::fill_keys(const unsigned long start,
                              const unsigned long end)
    {
        unsigned long  i;

        for (i = start; i < end; i++)
        {
            zobrist_key_t  new_key = random();
            keys[i] = new_key;
        }
    }

    void zobrist_t::ensure_size(const unsigned long new_num_keys)
    {
        if (num_keys < new_num_keys)
        {
            keys = (zobrist_key_t *)
                realloc(keys, sizeof(zobrist_key_t) * new_num_keys);
            fill_keys(num_keys, new_num_keys);
            num_keys = new_num_keys;
        }
    }

    zobrist_key_t zobrist_t::get_key(const unsigned long element)
    {
        ensure_size(element + 1);
        return keys[element];
    }

    void print_zobrist_key(ostream &stream, const zobrist_key_t key)
    {
        ios_base::fmtflags  flags;

        flags = stream.flags();
        stream << hex << showbase << ((unsigned long) key);
        stream.flags(flags);
    }

}

#endif // ZOBRIST_CC
