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

#ifndef HST_DIVERGENT_CC
#define HST_DIVERGENT_CC

#include <stack>

#include <hst/types.hh>
#include <hst/lts.hh>

using namespace std;

#ifndef DEBUG_DIVERGENCE
#define DEBUG_DIVERGENCE 0
#endif

namespace hst
{
    /*
     * Finds the set of divergent nodes in an LTS for a particular
     * event.  A node is divergent if it is possible to execute an
     * infinite sequence of that event from it; this is encoded in an
     * LTS by a cycle that is reachable from the node.  We use an
     * algorithm based on depth-first search to find the divergent
     * nodes in an LTS; this algorithm was devised by Michael
     * Goldsmith and is described in [Roscoe94, §2.2].
     *
     * [Roscoe94] A. W. Roscoe.  “Model-checking CSP”.  In AW Roscoe,
     *    editor, /A classical mind: Essays in honour of CAR Hoare/,
     *    pp353–378.  Prentice-Hall, 1994.
     */

    enum opcode
    {
        CHECK,
        FINISH
    };

    void lts_t::divergent_nodes
    (event_t event, stateset_t &divergent) const
    {
        /*
         * Initially we have to check every node in the LTS.
         */

        stateset_t  to_check(from_states_begin(), from_states_end());

        /*
         * Initialize these sets to ∅.
         */

        stateset_t  result;
        stateset_t  checking;
        stateset_t::iterator  it;

        /*
         * Set up our work queue.  It's actually a stack since we're
         * doing a DFS, but we'll still call it a queue.
         */

        typedef pair<opcode, state_t>  operation;

        /*
         * Loop for as long as we have nodes to check.
         */

        for (it = to_check.begin();
             it != to_check.end(); it = to_check.begin())
        {
            state_t  root = *it;

#if DEBUG_DIVERGENCE
            cerr << "Starting a DFS from " << root << "." << endl;
#endif

            /*
             * Start a DFS from this next node.
             */

            stack<operation>  queue;
            queue.push(operation(CHECK, root));

            while (!queue.empty())
            {
                /*
                 * Extract the next operation.
                 */

                operation  op = queue.top();
                queue.pop();
                state_t  state = op.second;

                if (op.first == FINISH)
                {
#if DEBUG_DIVERGENCE
                    cerr << "  Finishing " << state << endl;
#endif
                    /*
                     * We've reached a FINISH op, which means that
                     * we've checked all of this state's successors
                     * without finding a cycle.
                     */

                    to_check -= state;
                    checking -= state;
                } else if (op.first == CHECK) {
#if DEBUG_DIVERGENCE
                    cerr << "  Checking " << state << endl;
#endif

                    /*
                     * If this is a state that we're already checking,
                     * or that has already been marked divergent, then
                     * everything on the current DFS chain (i.e.,
                     * those states in [checking]) is divergent.  Mark
                     * them as such in [divergent].  Then, abort this
                     * DFS.
                     */

                    if (checking.contains(state) ||
                        result.contains(state))
                    {
#if DEBUG_DIVERGENCE
                        cerr << "Reached cycle in DFS chain.  "
                                "Marking nodes as divergent: " 
                             << checking << endl;
#endif

                        result |= checking;
                        to_check -= checking;
                        checking.clear();
                        break;
                    }

                    /*
                     * If we've haven't already seen this node, we
                     * need to add its successors to the DFS stack.
                     * We have more work to do for this state once all
                     * of its successors have been checked, so we
                     * should also add a FINISH op.  Since the queue
                     * is actually a stack, the FINISH op is pushed on
                     * first.
                     */

                    if (to_check.contains(state))
                    {
                        event_target_iterator  et_it;

                        checking += state;
                        queue.push(operation(FINISH, state));

                        for (et_it = event_targets_begin(state, event);
                             et_it != event_targets_end(state, event);
                             ++et_it)
                        {
                            queue.push(operation(CHECK, *et_it));
                        }
                    }
                }  /* op == CHECK */
            }  /* DFS while loop */

        }  /* to_check while loop */

        divergent.swap(result);
    }
}

#endif // HST_DIVERGENT_CC
