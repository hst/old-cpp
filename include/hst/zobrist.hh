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

#ifndef HST_ZOBRIST_HH
#define HST_ZOBRIST_HH

#include <cstdlib>
#include <iostream>

/*
 * Routines for dealing with Zobrist hashes.  Since the number of
 * states and events that we need isn't known in advance, we can't
 * precompute a table of Zobrist keys.  Instead, we must dynamically
 * generate the table as needed, using random numbers for each key.
 *
 * We further assume that the state and event types will be dense —
 * that there won't be large expanses of state or event numbers that
 * are unused.  We therefore use a simple linear array to store the
 * Zobrist keys, rather than a hash table or other sparse map.  We
 * maintain an invariant that the key array will always be full —
 * every allocated entry will have a key assigned, even if that key
 * has not been used yet.
 */

#define HST_INITIAL_KEY_ARRAY_SIZE  16

using namespace std;

namespace hst
{
    typedef unsigned long  zobrist_key_t;

    class zobrist_t
    {
    private:
        /*
         * The array of key values
         */

        zobrist_key_t  *keys;

        /*
         * The number of keys currently in the array
         */
        unsigned long  num_keys;

        void fill_keys(const unsigned long start,
                       const unsigned long end);
        void ensure_size(const unsigned long new_num_keys);

    public:
        zobrist_t()
        {
            keys = (zobrist_key_t *)
                malloc(sizeof(zobrist_key_t) * HST_INITIAL_KEY_ARRAY_SIZE);
            fill_keys(0, HST_INITIAL_KEY_ARRAY_SIZE);
            num_keys = HST_INITIAL_KEY_ARRAY_SIZE;
        }

        ~zobrist_t()
        {
            free(keys);
        }

        zobrist_key_t get_key(const unsigned long element);
    };

    void print_zobrist_key(ostream &stream, const zobrist_key_t key);
}

#endif // HST_ZOBRIST_HH
