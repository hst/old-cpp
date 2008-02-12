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

#ifndef HST_TYPES_HH
#define HST_TYPES_HH

#include <tr1/memory>

#include <hst/intset.hh>

/*
 * We will treat -1 (0xFFFFFFFF) as an error code, and will never
 * create a state or event with this number.
 */

#define HST_ERROR_STATE  ((hst::state_t) (-1))
#define HST_ERROR_EVENT  ((hst::event_t) (-1))

using namespace std;

namespace hst
{
    typedef unsigned long  state_t;

    typedef intset_t   stateset_t;
    typedef intset_p   stateset_p;
    typedef intset_cp  stateset_cp;

    struct state_t_hasher
    {
        unsigned long operator () (const state_t state) const
        {
            return state;
        }
    };

    typedef unsigned long  event_t;

    typedef intset_t   alphabet_t;
    typedef intset_p   alphabet_p;
    typedef intset_cp  alphabet_cp;

    struct event_t_hasher
    {
        unsigned long operator () (const event_t event) const
        {
            return event;
        }
    };

    typedef pair<state_t, state_t>           state_state_t;
    typedef shared_ptr<state_state_t>        state_state_p;
    typedef shared_ptr<const state_state_t>  state_state_cp;

    typedef pair<event_t, state_t>           event_state_t;
    typedef shared_ptr<event_state_t>        event_state_p;
    typedef shared_ptr<const event_state_t>  event_state_cp;

    struct string_hasher
    {
        unsigned long operator () (const string &str) const
        {
            unsigned long  hash = 0L;
            for (string::const_iterator it = str.begin();
                 it != str.end(); ++it)
            {
                hash += *it;
                hash += (hash << 10);
                hash ^= (hash >> 6);
            }

            hash += (hash << 3);
            hash ^= (hash >> 11);
            hash += (hash << 15);

            return hash;
        }
    };
}

#endif // HST_TYPES_H
