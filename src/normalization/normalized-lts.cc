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

#ifndef HST_NORMALIZED_LTS_CC
#define HST_NORMALIZED_LTS_CC

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>

using namespace std;

namespace hst
{
    state_t normalized_lts_t::get_normalized_state(stateset_cp set)
    {
        set_state_map_t::iterator  it;
        state_t  result;

        // Try to find this set.
        it = states.find(set);

        if (it == states.end())
        {
            // This set isn't in the graph, so let's create a new
            // state for it.

            result = _normalized.add_state("");
            states.insert(make_pair(set, result));
            sets.insert(make_pair(result, set));
        } else {
            result = it->second;
        }

        return result;
    }

    stateset_cp normalized_lts_t::get_normalized_set
    (state_t state) const
    {
        state_set_map_t::const_iterator  it;

        // Try to find this state.
        it = sets.find(state);

        if (it == sets.end())
        {
            // This set isn't in the graph, so we return a NULL
            // pointer.

            stateset_cp  result;
            return result;
        } else {
            return it->second;
        }
    }

    ostream &operator <<
    (ostream &stream, const normalized_lts_t &normalized)
    {
        bool  first;

        stream << "{" << endl;

        /*
         * First print the mapping from normalized states to sets of
         * source states.
         */

        stream << "  mapping {";

        first = true;
        normalized_lts_t::states_iterator  s_it;

        for (s_it = normalized.states_begin();
             s_it != normalized.states_end();
             ++s_it)
        {
            state_t  state = *s_it;

            if (first)
            {
                stream << endl << "    ";
                first = false;
            } else {
                stream << "," << endl << "    ";
            }

            stateset_cp  set = normalized.get_normalized_set(state);

            stream << state << "=" << *set;
        }

        stream << endl << "  }" << endl;

        /*
         * Then print the normalized LTS's edges.
         */

        stream << "  edges " << normalized.normalized()
               << endl << "}" << endl;
    }

}

#endif // HST_NORMALIZED_LTS_CC
