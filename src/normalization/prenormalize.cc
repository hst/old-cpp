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

#ifndef HST_PRENORMALIZE_CC
#define HST_PRENORMALIZE_CC

#include <hst/types.hh>
#include <hst/event-stateset-map.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>

using namespace std;

#ifndef DEBUG_PRENORMALIZE
#define DEBUG_PRENORMALIZE 0
#endif

namespace hst
{
    state_t normalized_lts_t::prenormalize(state_t source_state)
    {
        state_t     normalized_source;

        /*
         * We need to find the τ-closure of the [source_state] before
         * we can prenormalize.
         */

        {
            stateset_t  source_set;
            stateset_p  source_closure(new stateset_t);

            source_set += source_state;

#if DEBUG_PRENORMALIZE
            cerr << "Prenormalizing from state " << source_state
                 << "." << endl;
#endif

            _source->closure(_tau, *source_closure, source_set);
            normalized_source = get_normalized_state(source_closure);

#if DEBUG_PRENORMALIZE
            cerr << "Tau closure: " << *source_closure << endl
                 << "Normalized state = " << normalized_source << endl;
#endif

            /*
             * If we've already prenormalized this target state, just
             * return it.
             */

            if (prenormalized.contains(normalized_source))
            {
#if DEBUG_PRENORMALIZE
                cerr << normalized_closure
                     << " is already prenormalized." << endl;
#endif

                return normalized_source;
            }

        }


        /*
         * Loop through the [pending] set, prenormalizing each
         * normalized state that we encounter.
         */

        stateset_t  pending;
        pending += normalized_source;

        while (pending.size() != 0)
        {
            state_t  next = *(pending.begin());

            /*
             * Process this node.  First, we retrieve the set of
             * source states for this normalized node.
             */

            stateset_cp  next_set = get_normalized_set(next);

#if DEBUG_PRENORMALIZE
            cerr << "---" << endl
                 << "Examining state " << next
                 << "/" << *next_set << endl;
#endif

            /*
             * To create the successor edges, we first find all of the
             * non-τ targets of the source set, and place them in the
             * accumulator.
             */

            event_stateset_map_t  accumulator;
            stateset_t::iterator  s_it;

            for (s_it = next_set->begin();
                 s_it != next_set->end();
                 ++s_it)
            {
                state_t  state = *s_it;

#if DEBUG_PRENORMALIZE
                cerr << "  Finding successor nodes for "
                     << state << endl;
#endif

                lts_t::state_pairs_iterator  sp_it;

                for (sp_it = _source->state_pairs_begin(state);
                     sp_it != _source->state_pairs_end(state);
                     ++sp_it)
                {
                    event_t  event = sp_it->first;
                    state_t  to_state = sp_it->second;

#if DEBUG_PRENORMALIZE
                    cerr << "    --" << event << "--> "
                         << to_state << endl;
#endif

                    if (event == _tau)
                    {
                        /*
                         * We don't include the τ events, since these
                         * will be taken care of when we take the
                         * τ-closure of the whole lot.
                         */

#if DEBUG_PRENORMALIZE
                        cerr << "      (Skipping taus)" << endl;
#endif
                    } else {
                        accumulator.add(event, to_state);
                    }
                }

            }

            /*
             * Next we create successor edges for the normalized state
             * corresponding to [from_set], according to the contents
             * of the accumulator's [event_map].  Each target stateset
             * is τ-closed before adding the normalized edge.
             */

#if DEBUG_PRENORMALIZE
            cerr << "  Adding new edges." << endl;
#endif

            /*
             * Loop through the events in the accumulator's event map.
             */

            const event_stateset_map_t  &c_accumulator = accumulator;
            event_stateset_map_t::events_iterator  e_it;

            for (e_it = c_accumulator.events_begin();
                 e_it != c_accumulator.events_end();
                 ++e_it)
            {
                event_t  event = *e_it;

                /*
                 * Find the τ-closure of this event's state set.
                 */

                stateset_cp  event_image = c_accumulator.get(event);
                stateset_p   closure(new stateset_t);

                _source->closure(_tau, *closure, *event_image);

#if DEBUG_PRENORMALIZE
                cerr << "    Event: " << event << endl
                     << "      Successors:  " << *event_image << endl
                     << "      Tau closure: " << *closure << endl;
#endif

                /*
                 * Determine the normalized LTS state for this
                 * τ-closure set.
                 */

                state_t  normalized_closure =
                    get_normalized_state(closure);

#if DEBUG_PRENORMALIZE
                cerr << "      Normalized state: "
                     << normalized_closure << endl;
#endif

                /*
                 * Add an edge in the normalized LTS for this event.
                 */

#if DEBUG_PRENORMALIZE
                cerr << "      Adding edge "
                     << next << " --"
                     << event << "--> "
                     << normalized_closure << endl;
#endif

                _normalized.add_edge(next, event, normalized_closure);

                /*
                 * If the target state has not already been
                 * prenormalized, add it to the [pending] queue.
                 */

                if (!prenormalized.contains(normalized_closure))
                {
#if DEBUG_PRENORMALIZE
                    cerr << "      " << normalized_closure
                         << " is new; adding to pending" << endl;
#endif

                    pending += normalized_closure;
                }
            }

            /*
             * We've finished with this state, so remove it from the
             * [pending] set and add it to the [finished] set.
             */

#if DEBUG_PRENORMALIZE
            cerr << "  Moving " << next
                 << " from \"pending\" to \"finished\"." << endl;
#endif

            pending -= next;
            prenormalized += next;
        }

        return normalized_source;
    }
}

#endif // HST_PRENORMALIZE_CC
