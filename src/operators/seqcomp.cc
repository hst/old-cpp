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
    static
    void do_seqcomp(csp_t &csp, state_t dest, state_t P, state_t Q)
    {
        lts_t  &_lts = *csp.lts();

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

        bool  found_a_tau = false;
        lts_t::state_pairs_iterator  sp_it;

        /*
         * First walk through P's outgoing edges.
         */

        for (sp_it = _lts.state_pairs_begin(P);
             sp_it != _lts.state_pairs_end(P); ++sp_it)
        {
            event_t  E       = sp_it->first;
            state_t  P_prime = sp_it->second;

            if (E == csp.tick())
            {
                /*
                 * If the event is a ✓, then P has finished, and Q is
                 * allowed to begin.  The ✓ is not externally visible,
                 * however; it is translated into a τ.  This means we
                 * need to create a transition for
                 *
                 *   P ; Q =τ=> Q
                 */

                _lts.add_edge(dest, csp.tau(), Q);

                found_a_tau = true;
            } else {
                /*
                 * If the event is not a ✓, then the event does not
                 * resolve the composition; P' is now in sequence with
                 * Q.  We need to create a transition for
                 *
                 *   P ; Q =E=> P' ; Q
                 */

                state_t  P_prime_seqcomp_Q =
                    csp.add_seqcomp(P_prime, Q);
                _lts.add_edge(dest, E, P_prime_seqcomp_Q);

                if (E == csp.tau())
                {
                    found_a_tau = true;
                }
            }
        }

        /*
         * If there aren't any τ edges for the sequential composition,
         * then it will have the same acceptances as P, but with any
         * ✓s removed.  (Of course, if there are any ✓s in the
         * acceptance sets, then we'll have created a τ previously...)
         */

        if (!found_a_tau)
        {
            alphabet_set_cp  P_alphas = _lts.get_acceptances(P);

            for (alphabet_set_t::iterator Pa_it = P_alphas->begin();
                 Pa_it != P_alphas->end();
                 ++Pa_it)
            {
                alphabet_t  acceptance;

                acceptance |= *Pa_it;
                acceptance -= csp.tick();

                _lts.add_acceptance(dest, acceptance);
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }

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
            save_memoized_process(key.str(), dest);
            do_seqcomp(*this, dest, P, Q);
        }

        return dest;
    }

    void csp_t::seqcomp(state_t dest, state_t P, state_t Q)
    {
        ostringstream  key;
        state_t        old_dest;

        // Create the memoization key.
        key << P << ";" << Q;

        old_dest = lookup_memoized_process(key.str());
        if (old_dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            save_memoized_process(key.str(), dest);
            do_seqcomp(*this, dest, P, Q);
        } else {
            // We've already created this process, so let's just add a
            // single τ process to the previously calculated state.
            _lts.add_edge(dest, _tau, old_dest);
            _lts.finalize(dest);
        }
    }
}

#endif // HST_CSP_SEQCOMP_CC
