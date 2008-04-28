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

#ifndef HST_CSP_HIDE_CC
#define HST_CSP_HIDE_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    static
    void do_hide(csp_t &csp, state_t dest, state_t P, alphabet_t &alpha)
    {
        lts_t  &_lts = *csp.lts();

#if HST_CSP_DEBUG
        cerr << "Hide " << dest
             << " = " << P << " \\ " << alpha << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  ‘P’ should
         * be finalized, since we need its initial events to
         * calculate [P \ α].
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
                 * If the event is a ✓, then the hidden process can
                 * also perform a ✓, but it must lead directly to
                 * STOP.  This means we need to create a transition
                 * for
                 *
                 *   P \ α =✓=> STOP
                 */

                _lts.add_edge(dest, E, csp.stop());
            } else {
                /*
                 * If the event is not ✓, we can still execute it, but
                 * we must first see if it's in the hiding alphabet.
                 * If it is, it should be turned into a τ.
                 */

                if (alpha.contains(E))
                    E = csp.tau();

                state_t  P_prime_hide =
                    csp.add_hide(P_prime, alpha);
                _lts.add_edge(dest, E, P_prime_hide);

                if (E == csp.tau())
                {
                    found_a_tau = true;
                }
            }
        }

        /*
         * If we didn't create any τ events for the hiding, then it
         * will have the same set of acceptances as P.  Otherwise, it
         * will have no acceptances at all.
         */

        if (!found_a_tau)
        {
            alphabet_set_cp  P_alphas = _lts.get_acceptances(P);

            for (alphabet_set_t::iterator Pa_it = P_alphas->begin();
                 Pa_it != P_alphas->end();
                 ++Pa_it)
            {
                _lts.add_acceptance(dest, *Pa_it);
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }

    state_t csp_t::add_hide(state_t P, alphabet_t &alpha)
    {
        ostringstream  key;
        state_t        dest;

        // Create the memoization key.
        key << P << "\\" << alpha;

        dest = lookup_memoized_process(key.str());
        if (dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            dest = add_temp_process();
            save_memoized_process(key.str(), dest);
            do_hide(*this, dest, P, alpha);
        }

        return dest;
    }

    void csp_t::hide(state_t dest, state_t P, alphabet_t &alpha)
    {
        ostringstream  key;
        state_t        old_dest;

        // Create the memoization key.
        key << P << "\\" << alpha;

        old_dest = lookup_memoized_process(key.str());
        if (old_dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            save_memoized_process(key.str(), dest);
            do_hide(*this, dest, P, alpha);
        } else {
            // We've already created this process, so let's just add a
            // single τ process to the previously calculated state.
            _lts.add_edge(dest, _tau, old_dest);
            _lts.finalize(dest);
        }
    }
}

#endif // HST_CSP_HIDE_CC
