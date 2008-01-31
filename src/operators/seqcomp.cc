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

#ifndef HST_CSP_SEQCOMP_CC
#define HST_CSP_SEQCOMP_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    state_t csp_t::add_seqcomp(state_t P, state_t Q)
    {
        ostringstream  key;
        state_t        dest;

        // Create the memoization key.
        key << P << ";" << Q;

        dest = lookup_memoized_process(key.str());
        if (dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            dest = add_temp_process();
            seqcomp(dest, P, Q);
            save_memoized_process(key.str(), dest);
        }

        return dest;
    }

    void csp_t::seqcomp(state_t dest, state_t P, state_t Q)
    {
#if HST_CSP_DEBUG
        cerr << "Seqcomp " << dest
             << " = " << P << " ; " << Q << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  ‘P’ should be
         * finalized, since we need its initial events to calculate
         * [P;Q]; ‘Q’, on the other hand, does not need to be
         * finalized.
         */

        REQUIRE_NOT_FINALIZED(dest);
        REQUIRE_FINALIZED(P);

        /*
         * All of the firing rules are of the form
         *
         *   P =E=> P' ⇒ something
         *
         * We add the appropriate transitions by walking through each
         * (E,P') pair from P.  The consequent is different depending
         * on whether the event is a ✓ or not.
         */

        lts_t::state_pairs_iterator  sp_it;

        /*
         * First walk through P's outgoing edges.
         */

        for (sp_it = _lts.state_pairs_begin(P);
             sp_it != _lts.state_pairs_end(P); ++sp_it)
        {
            event_t  E       = sp_it->first;
            state_t  P_prime = sp_it->second;

            if (E == _tick)
            {
                /*
                 * If the event is a ✓, then P has finished, and Q is
                 * allowed to begin.  The ✓ is not externally visible,
                 * however; it is translated into a τ.  This means we
                 * need to create a transition for
                 *
                 *   P ; Q =τ=> Q
                 */

                _lts.add_edge(dest, _tau, Q);
            } else {
                /*
                 * If the event is not a ✓, then the event does not
                 * resolve the composition; P' is now in sequence with
                 * Q.  We need to create a transition for
                 *
                 *   P ; Q =E=> P' ; Q
                 */

                state_t  P_prime_seqcomp_Q =
                    add_seqcomp(P_prime, Q);
                _lts.add_edge(dest, E, P_prime_seqcomp_Q);
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }
}

#endif // HST_CSP_SEQCOMP_CC
