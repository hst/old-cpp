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

#ifndef HST_CSP_INTERFACE_PARALLEL_CC
#define HST_CSP_INTERFACE_PARALLEL_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    static
    void do_interface_parallel
    (csp_t &csp, state_t dest, state_t P, alphabet_t &alpha, state_t Q)
    {
        lts_t  &_lts = *csp.lts();

#if HST_CSP_DEBUG
        cerr << "Interface parallel " << dest << " = "
             << P << " [|"
             << alpha << "|] "
             << Q << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  ‘P’ and ‘Q’ should
         * be finalized, since we need their initial events to
         * calculate [P〚A〛Q].
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
         * (E,P') pair from P, and each (E,Q') pair from Q.  Some of
         * the firing rules advance P or Q without advancing the
         * other; this can only happen if the event in not in the
         * synchronization alphabet.
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

            /*
             * First check if the event needs to be synchronized.  (✓
             * must always be synchronized, since both processes must
             * agree to terminate before the parallel composition can
             * terminate.)
             */

            if (E == csp.tick() || alpha.contains(E))
            {
                /*
                 * The event must be synchronized, so we add an edge
                 * for each Q transition that is available with this
                 * event.  If there are no Q transitions for this
                 * event, the parallel composition cannot execute the
                 * event.
                 */

                lts_t::event_target_iterator  et_it;

                for (et_it = _lts.event_targets_begin(Q, E);
                     et_it != _lts.event_targets_end(Q, E); ++et_it)
                {
                    state_t  Q_prime = *et_it;

                    /*
                     * Given P =E=> P' and Q =E=> Q', which we have
                     * now just found, we need to create a transition
                     * for
                     *
                     *   P〚A〛Q =E=> P'〚A〛Q'
                     */

                    state_t  P_prime_parallel_Q_prime =
                        csp.add_interface_parallel(P_prime, alpha, Q_prime);
                    _lts.add_edge(dest, E, P_prime_parallel_Q_prime);
                }

            } else {
                /*
                 * [P〚A〛Q] can perform any action of P not in the
                 * synchronization alphabet (including τ), after which
                 * it behaves like [P'〚A〛Q].  This means we need to
                 * create a transition for
                 *
                 *   P〚A〛Q =E=> P'〚A〛Q
                 */

                state_t  P_prime_parallel_Q =
                    csp.add_interface_parallel(P_prime, alpha, Q);
                _lts.add_edge(dest, E, P_prime_parallel_Q);
            }
        }

        /*
         * Now repeat basically the same process for Q's outgoing
         * edges.
         */

        for (sp_it = _lts.state_pairs_begin(Q);
             sp_it != _lts.state_pairs_end(Q); ++sp_it)
        {
            event_t  E       = sp_it->first;
            state_t  Q_prime = sp_it->second;

            /*
             * First check if the event needs to be synchronized.  (✓
             * must always be synchronized, since both processes must
             * agree to terminate before the parallel composition can
             * terminate.)
             */

            if (E == csp.tick() || alpha.contains(E))
            {
                /*
                 * We'll have already found the synchronized events
                 * while processing the P/P' transitions, so we don't
                 * have to process them while processing the Q/Q'
                 * transitions.
                 */

            } else {
                /*
                 * [P〚A〛Q] can perform any action of Q not in the
                 * synchronization alphabet (including τ), after which
                 * it behaves like [P〚A〛Q'].  This means we need to
                 * create a transition for
                 *
                 *   P〚A〛Q =E=> P〚A〛Q'
                 */

                state_t  P_parallel_Q_prime =
                    csp.add_interface_parallel(P, alpha, Q_prime);
                _lts.add_edge(dest, E, P_parallel_Q_prime);
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }

    state_t csp_t::add_interface_parallel
    (state_t P, alphabet_t &alpha, state_t Q)
    {
        ostringstream  key;
        state_t        dest;

        // Interface parallel is commutative, so always memoize with
        // the lower-numbered process first.
        if (Q < P) std::swap(P,Q);

        // Create the memoization key.
        key << P << "[|" << alpha << "|]" << Q;

        dest = lookup_memoized_process(key.str());
        if (dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            dest = add_temp_process();
            save_memoized_process(key.str(), dest);
            do_interface_parallel(*this, dest, P, alpha, Q);
        }

        return dest;
    }

    void csp_t::interface_parallel
    (state_t dest, state_t P, alphabet_t &alpha, state_t Q)
    {
        ostringstream  key;
        state_t        old_dest;

        // Interface parallel is commutative, so always memoize with
        // the lower-numbered process first.
        if (Q < P) std::swap(P,Q);

        // Create the memoization key.
        key << P << "[|" << alpha << "|]" << Q;

        old_dest = lookup_memoized_process(key.str());
        if (old_dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            save_memoized_process(key.str(), dest);
            do_interface_parallel(*this, dest, P, alpha, Q);
        } else {
            // We've already create this process, so let's just add a
            // single τ process to the previously calculated state.
            _lts.add_edge(dest, _tau, old_dest);
            _lts.finalize(dest);
        }
    }
}

#endif // HST_CSP_INTERFACE_PARALLEL_CC
