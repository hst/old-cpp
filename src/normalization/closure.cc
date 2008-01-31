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

#ifndef HST_CLOSURE_CC
#define HST_CLOSURE_CC

#include <hst/types.hh>
#include <hst/lts.hh>

using namespace std;

namespace hst
{
    /*
     * Finds the closure of a set of LTS nodes for a particular event.
     * The closure is the set of nodes that can be reached from any of
     * the initial nodes by following any sequence of that event.  The
     * event will usually be τ.
     *
     * The pseudo-code algorithm is:
     *
     * CLOSURE(initial_nodes, event): closure_nodes
     *   seen = ∅
     *   closure_nodes = ∅
     *   queue = initial_nodes
     *   for x ∈ queue:
     *     seen ∪= {x}
     *     closure_nodes ∪= {x}
     *     queue ∖= {x}
     *     for y ∈ afters(x, event):
     *       if y ∉ seen:
     *         queue ∪= {y}
     */

    void lts_t::closure
    (event_t event, stateset_t &closure,
     const stateset_t &initial) const
    {
        /*
         * Initialize seen_states and result to ∅.
         */

        stateset_t  seen_states;
        stateset_t  result;

        /*
         * Add the initial states to the queue of states to check.
         */

        stateset_t  queue(initial);

        stateset_t::iterator  it;

        for (it = queue.begin(); it != queue.end(); it = queue.begin())
        {
            state_t  state = *it;

            /*
             * Remove the state we just found from the queue, and add it
             * to the result and seen_states sets.
             */

            queue -= state;
            seen_states += state;
            result += state;

            /*
             * Walk through the states we can reach from here by
             * following a single [event].
             */

            event_target_iterator  et_it;

            for (et_it = event_targets_begin(state, event);
                 et_it != event_targets_end(state, event); ++et_it)
            {
                /*
                 * We want to add this state to the queue if we
                 * haven't already checked it.
                 */

                if (!seen_states.contains(*et_it))
                    queue += *et_it;
            }
        }

        /*
         * We're all good; return the result.
         */

        closure.swap(result);
    }
}

#endif // HST_CLOSURE_CC
