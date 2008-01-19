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
    void csp_t::extchoice(state_t dest,
                          state_t P, state_t Q)
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

        lts_t::state_events_iterator  se_it;

        /*
         * First walk through P's outgoing edges.
         */

        for (se_it = _lts.state_events_begin(P);
             se_it != _lts.state_events_end(P); ++se_it)
        {
            event_t  E = *se_it;
            lts_t::event_target_iterator  et_it;

            for (et_it = _lts.event_targets_begin(P, E);
                 et_it != _lts.event_targets_end(P, E); ++et_it)
            {
                state_t  P_prime = *et_it;

                if (E == _tau)
                {
                    /*
                     * If the event is a τ, then it does *not* resolve
                     * the choice; P' is available, but so is Q.  This
                     * means we need to create a transition for
                     *
                     *   P □ Q =τ=> P' □ Q
                     */

                    state_t  P_prime_extchoice_Q = add_temp_process();
                    extchoice(P_prime_extchoice_Q, P_prime, Q);
                    _lts.add_edge(dest, E, P_prime_extchoice_Q);
                } else {
                    /*
                     * If the event is not τ, then it resolves the
                     * choice; the alternative is no longer available.
                     * We need to create a transition for
                     *
                     *   P □ Q =E=> P'
                     */

                    _lts.add_edge(dest, E, P_prime);
                }
            }
        }

        /*
         * Now repeat the same process for Q's outgoing edges.
         */

        for (se_it = _lts.state_events_begin(Q);
             se_it != _lts.state_events_end(Q); ++se_it)
        {
            event_t  E = *se_it;
            lts_t::event_target_iterator  et_it;

            for (et_it = _lts.event_targets_begin(Q, E);
                 et_it != _lts.event_targets_end(Q, E); ++et_it)
            {
                state_t  Q_prime = *et_it;

                if (E == _tau)
                {
                    /*
                     * If the event is a τ, then it does *not* resolve
                     * the choice; Q' is available, but so is P.  This
                     * means we need to create a transition for
                     *
                     *   P □ Q =τ=> P □ Q'
                     */

                    state_t  P_extchoice_Q_prime = add_temp_process();
                    extchoice(P_extchoice_Q_prime, P, Q_prime);
                    _lts.add_edge(dest, E, P_extchoice_Q_prime);
                } else {
                    /*
                     * If the event is not τ, then it resolves the
                     * choice; the alternative is no longer available.
                     * We need to create a transition for
                     *
                     *   P □ Q =E=> Q'
                     */

                    _lts.add_edge(dest, E, Q_prime);
                }
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }
}

#endif // HST_CSP_EXTCHOICE_CC
