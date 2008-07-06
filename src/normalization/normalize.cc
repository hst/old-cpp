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

#ifndef HST_NORMALIZE_CC
#define HST_NORMALIZE_CC

#include <assert.h>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/types.hh>
#include <hst/equivalence.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>

using namespace std;

#ifndef DEBUG_NORMALIZE
#define DEBUG_NORMALIZE 0
#endif

namespace hst
{
    void normalized_lts_t::normalize()
    {
        if (_stage != PRENORMALIZING)
        {
#if DEBUG_PRENORMALIZE
            cerr << "Already fully normalized!" << endl;
            return;
#endif
        }

        // First, bisimulate the prenormalized LTS.

        equivalences_t  equiv;
        _normalized.bisimulate(equiv, _semantic_model);

        // Next, compose the initials map with the bisimulation
        // relation.

        initial_map_t  new_initials;
        initial_map_t::const_iterator  im_it;

        for (im_it = initial_map.begin();
             im_it != initial_map.end();
             ++im_it)
        {
            state_t  initial_head = equiv.head(im_it->second);
            new_initials.insert(make_pair(im_it->first, initial_head));
        }

        // Now, create a new normalized LTS using this bisimulation.

        lts_t            new_lts;
        set_state_map_t  new_states;
        state_set_map_t  new_sets;

        // Create new nodes for the head of each equivalence class.

        typedef judy_map_l<state_t, state_t, state_t_hasher>
            new_head_map_t;
        new_head_map_t  new_head_map;

        equivalences_t::heads_iterator  h_it;

        for (h_it = equiv.heads_begin();
             h_it != equiv.heads_end();
             ++h_it)
        {
            state_t  head = *h_it;
            state_t  new_node = new_lts.add_state("");
            stateset_cp  set(new stateset_t);

            new_head_map.insert(make_pair(head, new_node));
            new_sets.insert(make_pair(new_node, set));
        }

        // Loop through each prenormalized state, adding its behavior
        // to the new LTS node corresponding to its equivalence class.

        equivalences_t::pairs_iterator  p_it;

        for (p_it = equiv.pairs_begin();
             p_it != equiv.pairs_end();
             ++p_it)
        {
            state_t  state = p_it->first;
            state_t  head  = p_it->second;

            // We've created new nodes for all of the heads, so this
            // find() call should always succeed.

            state_t  new_node = new_head_map.find(head)->second;

            // This normalized state corresponds to a set of source
            // states; these source states should be added to
            // new_sets.

            stateset_cp  old_set = get_normalized_set(state);
            state_set_map_t::iterator  ssm_it = new_sets.find(new_node);
            stateset_t  *new_set =
                const_cast<stateset_t *>(ssm_it->second.get());

            *new_set |= *old_set;

            // The old source set should map to this equivalence
            // class's new node.

            new_states.insert(make_pair(old_set, new_node));

            // Finally, all of the old normalized state's edges and
            // acceptances should be added to the new state.

            for (lts_t::state_pairs_iterator
                     sp_it = _normalized.state_pairs_begin(state);
                 sp_it != _normalized.state_pairs_end(state);
                 ++sp_it)
            {
                event_t  event = sp_it->first;
                state_t  old_target = sp_it->second;
                state_t  target_head = equiv.head(old_target);
                state_t  new_target =
                    new_head_map.find(target_head)->second;

                new_lts.add_edge(new_node, event, new_target);
            }

            for (lts_t::acceptances_iterator
                     a_it = _normalized.acceptances_begin();
                 a_it != _normalized.acceptances_end();
                 ++a_it)
            {
                state_t  old_state = a_it->first;
                state_t  state_head = equiv.head(old_state);
                state_t  new_state =
                    new_head_map.find(state_head)->second;

                alphabet_set_cp  acceptance = a_it->second;
                for (alphabet_set_t::iterator
                         acc_it = acceptance->begin();
                     acc_it != acceptance->end();
                     ++acc_it)
                {
                    new_lts.add_acceptance(new_state, *acc_it);
                }
            }
        }

        // Sweet, we've created the new LTS and set/state mappings, so
        // swap them into place and reset the other necessary fields.

        _normalized.swap(new_lts);
        states.swap(new_states);
        sets.swap(new_sets);
        initial_map.swap(new_initials);
        prenormalized.clear();
        _stage = NORMALIZED;
    }
}

#endif // HST_NORMALIZE_CC
