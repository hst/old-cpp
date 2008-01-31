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

#ifndef HST_CSP_EXTCHOICE_CC
#define HST_CSP_EXTCHOICE_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    state_t csp_t::add_extchoice(state_t P, state_t Q)
    {
        ostringstream  key;
        state_t        dest;

        // Extchoice is commutative, so always memoize with the
        // lower-numbered process first.
        if (Q < P) std::swap(P,Q);

        // Create the memoization key.
        key << P << "[]" << Q;

        dest = lookup_memoized_process(key.str());
        if (dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            dest = add_temp_process();
            extchoice(dest, P, Q);
            save_memoized_process(key.str(), dest);
        }

        return dest;
    }

    void csp_t::extchoice(state_t dest, state_t P, state_t Q)
    {
#if HST_CSP_DEBUG
        cerr << "Extchoice " << dest
             << " = " << P << " [] " << Q << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  ‘P’ and ‘Q’ should
         * be finalized, since we need their initial events to
         * calculate [P□Q].
         */

        REQUIRE_NOT_FINALIZED(dest);
        REQUIRE_FINALIZED(P);
        REQUIRE_FINALIZED(Q);

        /*
         * All of the firing rules are of the form
         *
         *   P =E=> P' ⇒ something
         *
         * or
         *
         *   Q =E=> Q' ⇒ something
         *
         * We add the appropriate transitions by walking through each
         * (E,P') pair from P, and each (E,Q') pair from Q.  The
         * consequent is different depending on whether the event is a
         * τ or not.
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

            if (E == _tau)
            {
                /*
                 * If the event is a τ, then it does *not* resolve the
                 * choice; P' is available, but so is Q.  This means
                 * we need to create a transition for
                 *
                 *   P □ Q =τ=> P' □ Q
                 */

                state_t  P_prime_extchoice_Q =
                    add_extchoice(P_prime, Q);
                _lts.add_edge(dest, E, P_prime_extchoice_Q);
            } else {
                /*
                 * If the event is not τ, then it resolves the choice;
                 * the alternative is no longer available.  We need to
                 * create a transition for
                 *
                 *   P □ Q =E=> P'
                 */

                _lts.add_edge(dest, E, P_prime);
            }
        }

        /*
         * Now repeat the same process for Q's outgoing edges.
         */

        for (sp_it = _lts.state_pairs_begin(Q);
             sp_it != _lts.state_pairs_end(Q); ++sp_it)
        {
            event_t  E       = sp_it->first;
            state_t  Q_prime = sp_it->second;

            if (E == _tau)
            {
                /*
                 * If the event is a τ, then it does *not* resolve the
                 * choice; Q' is available, but so is P.  This means
                 * we need to create a transition for
                 *
                 *   P □ Q =τ=> P □ Q'
                 */

                state_t  P_extchoice_Q_prime =
                    add_extchoice(P, Q_prime);
                _lts.add_edge(dest, E, P_extchoice_Q_prime);
            } else {
                /*
                 * If the event is not τ, then it resolves the choice;
                 * the alternative is no longer available.  We need to
                 * create a transition for
                 *
                 *   P □ Q =E=> Q'
                 */

                _lts.add_edge(dest, E, Q_prime);
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }
}

#endif // HST_CSP_EXTCHOICE_CC
