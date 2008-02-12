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

#ifndef HST_BISIMULATE_CC
#define HST_BISIMULATE_CC

#include <assert.h>
#include <iostream>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/intset.hh>
#include <hst/types.hh>
#include <hst/equivalence.hh>
#include <hst/lts.hh>

using namespace std;

#ifndef DEBUG_BISIMULATE
#define DEBUG_BISIMULATE 0
#endif

namespace hst
{
    /**
     * We initialize the bisimulation relation so that any two states
     * that have the same set of initial events are equivalent.
     */

    static
    void initialize_equivalence(equivalences_t &equiv, const lts_t &lts)
    {
#if DEBUG_BISIMULATE
        cerr << "Initializing bisimulation relation" << endl;
#endif

        // The map stores the first state that we find with a
        // particular initials set.  Since we know that we walk
        // through the states in order, the first one we find is the
        // correct head of its equivalence class.

        typedef judy_map_l<alphabet_t, state_t, intset_t_hasher>
            map_t;

        map_t  initials_map;

        // Loop through each state in the LTS.

        state_t  state;

        for (state_t state = 0; state < lts.state_count(); state++)
        {
#if DEBUG_BISIMULATE
            cerr << "  Examining state " << state;
#endif

            // Get the initials alphabet for this state.

            alphabet_t alpha(lts.state_events_begin(state),
                             lts.state_events_end(state));

#if DEBUG_BISIMULATE
            cerr << " (initials = " << alpha << ")" << endl;
#endif

            // Try to find this alphabet in the initials_map.

            map_t::iterator  im_it = initials_map.find(alpha);

            if (im_it == initials_map.end())
            {
                // We haven't seen this alphabet yet.  It's going to
                // be its own head in the bisimulation relation.

#if DEBUG_BISIMULATE
                cerr << "    new equiv class" << endl;
#endif

                equiv.add(state, state);
                initials_map.insert(make_pair(alpha, state));
            } else {
                // We've seen this alphabet before.  Its entry in
                // initials_map should be this state's head.

#if DEBUG_BISIMULATE
                cerr << "    old equiv class = " << im_it->second << endl;
#endif

                equiv.add(state, im_it->second);
            }
        }
    }

    /**
     * Two states (which we assume are equivalent in [equiv]) should
     * continue to be equivalent if all of the targets from both
     * lead to states that are themselves equivalent.
     */

    static
    bool equivalent(const lts_t &lts, const equivalences_t &equiv,
                    state_t state1, state_t state2)
    {
        // If the two state numbers are identical, then they're
        // obviously still equivalent.

        if (state1 == state2)
            return true;

#if DEBUG_BISIMULATE
        cerr << "  " << state1 << " ?~ " << state2 << endl;
#endif

        // We should return true if the following predicate holds
        // (expressed in English above):
        //
        // ∀ s₁′,s₂′ · (s₁ e→ s₁′ ∧ s₂ e→ s₂′) ⇒ (s₁′ ∼ s₂′)
        //
        // To do this, we look for the negation:
        //
        // ∃ s₁′,s₂′ · (s₁ e→ s₁′ ∧ s₂ e→ s₂′ ∧ s₁′ !∼ s₂′)
        //
        // and return false if we ever find it.

        lts_t::state_pairs_iterator  sp_it;

        for (sp_it = lts.state_pairs_begin(state1);
             sp_it != lts.state_pairs_end(state1);
             ++sp_it)
        {
            event_t  event = sp_it->first;
            state_t  s1_prime = sp_it->second;

            lts_t::event_target_iterator  et_it;

            for (et_it = lts.event_targets_begin(state2, event);
                 et_it != lts.event_targets_end(state2, event);
                 ++et_it)
            {
                state_t  s2_prime = *et_it;

                // By looping through the events in this way, we now
                // have an s₁′ and s₂′ such that:
                //
                //   (s₁ e→ s₁′ ∧ s₂ e→ s₂′)
                //
                // so all we have to do is verify that
                //
                //   s₁′ ∼ s₂′
                //
                // If not, we've found an example of the negation and
                // should return false.

                if (equiv.equivalent(s1_prime, s2_prime))
                {
#if DEBUG_BISIMULATE
                    cerr << "    ("
                         << state1 << " --" << event << "--> " << s1_prime
                         << ")  ~ ("
                         << state2 << " --" << event << "--> " << s2_prime
                         << ")" << endl;
#endif
                } else {
#if DEBUG_BISIMULATE
                    cerr << "    ("
                         << state1 << " --" << event << "--> " << s1_prime
                         << ") !~ ("
                         << state2 << " --" << event << "--> " << s2_prime
                         << ")" << endl;
#endif

                    return false;
                }
            }
        }

        // We didn't find any negation, so the two nodes are
        // equivalent.

#if DEBUG_BISIMULATE
        cerr << "    Still equivalent." << endl;
#endif

        return true;
    }

    void lts_t::bisimulate(equivalences_t &equiv) const
    {
        // Initialize the equivalence classes.

        equivalences_t  prev_equiv;
        initialize_equivalence(prev_equiv, *this);

        // Iterate through the fixed-point algorithm.
        bool changed;

        do
        {
#if DEBUG_BISIMULATE
            cerr << endl << "---" << endl
                 << "New iteration.  "
                 << "Previous equivalence relation:" << endl
                 << prev_equiv << endl;
#endif

            // We don't want to start another iteration after this one
            // unless we find any changes.

            changed = false;

            // We're going to build up a new equivalence relation
            // based on the current one.

            equivalences_t  next_equiv;

            // Loop through each pair of states that were equivalent
            // before, verifying that they're still equivalent.
            // Separate any that are not equivalent to their head into
            // a new class.

            equivalences_t::heads_iterator  h_it;

            for (h_it = prev_equiv.heads_begin();
                 h_it != prev_equiv.heads_end();
                 ++h_it)
            {
                // If we find a non-equivalent member of this class,
                // we'll need to separate it out into a new class.
                // This new class will need a head, which will be the
                // first non-equivalent member we find.

                state_t  new_head = HST_ERROR_STATE;

                state_t  head = *h_it;
                stateset_cp  members = prev_equiv.members(head);
                stateset_t::iterator  m_it;

                for (m_it = members->begin();
                     m_it != members->end();
                     ++m_it)
                {
                    state_t  member = *m_it;

                    if (equivalent(*this, prev_equiv, head, member))
                    {
                        // This state is still equivalent to its head,
                        // so mark it as such in the new equivalence
                        // relation.

                        next_equiv.add(member, head);
                    } else {
                        // This state is not equivalent to its head.
                        // If necessary, create a new head state and
                        // mark this in the new relation.

                        if (new_head == HST_ERROR_STATE)
                            new_head = member;

                        next_equiv.add(member, new_head);

                        // This also means we need another fixed-point
                        // iteration.

                        changed = true;
                    }
                }
            }

            // Prepare for the next iteration by swapping the
            // equivalence relations.  This also has the effect of
            // freeing prev_equiv.

            prev_equiv.swap(next_equiv);

        } while (changed);

        // Once we fall out of the fixed-point loop, we've found the
        // final bisimulation relation.

#if DEBUG_BISIMULATE
            cerr << endl << "---" << endl
                 << "We're done.  "
                 << "Final equivalence relation:" << endl
                 << prev_equiv << endl;
#endif

        equiv.swap(prev_equiv);
    }
}

#endif // HST_BISIMULATE_CC
