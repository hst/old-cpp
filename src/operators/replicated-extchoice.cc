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

#ifndef HST_CSP_REPLICATED_EXTCHOICE_CC
#define HST_CSP_REPLICATED_EXTCHOICE_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    static
    void do_replicated_extchoice(csp_t &csp, state_t dest,
                                 const stateset_t &Ps)
    {
        lts_t  &_lts = *csp.lts();

#if HST_CSP_DEBUG
        cerr << "Replicated extchoice " << dest
             << " = [] " << Ps << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  Every state in
         * ‘Ps’ should be finalized, since we need their initial
         * events to calculate [□Ps].
         */

        REQUIRE_NOT_FINALIZED(dest);

        for (stateset_t::iterator P_it = Ps.begin();
             P_it != Ps.end();
             ++P_it)
        {
            REQUIRE_FINALIZED(*P_it);
        }

        /*
         * All of the firing rules are of the form
         *
         *   P =E=> P' ⇒ something
         *
         * We add the appropriate transitions by walking through each
         * (E,P') pair from Ps.  The consequent is different depending
         * on whether the event is a τ or not.
         */

        bool  found_a_tau = false;

        /*
         * Walk through each of the states in Ps.
         */

        for (stateset_t::iterator P_it = Ps.begin();
             P_it != Ps.end();
             ++P_it)
        {
            state_t  P = *P_it;

            /*
             * Walk through P's outgoing edges.
             */

            for (lts_t::state_pairs_iterator sp_it =
                     _lts.state_pairs_begin(P);
                 sp_it != _lts.state_pairs_end(P);
                 ++sp_it)
            {
                event_t  E       = sp_it->first;
                state_t  P_prime = sp_it->second;

                if (E == csp.tau())
                {
                    /*
                     * If the event is a τ, then it does *not* resolve
                     * the choice; P' is available, but so all of the
                     * other states in Ps.  This means we need to
                     * create a transition for
                     *
                     *   □ Ps =τ=> □ (Ps ∖ {P} ∪ {P'})
                     */

                    stateset_t  new_Ps(Ps);
                    new_Ps -= P;
                    new_Ps += P_prime;

                    state_t  repl_extchoice_prime =
                        csp.add_replicated_extchoice(new_Ps);
                    _lts.add_edge(dest, E, repl_extchoice_prime);

                    found_a_tau = true;
                } else {
                    /*
                     * If the event is not τ, then it resolves the
                     * choice; the alternative is no longer available.
                     * We need to create a transition for
                     *
                     *   □ Ps =E=> P'
                     */

                    _lts.add_edge(dest, E, P_prime);
                }
            }
        }

        /*
         * If there were any τ events for the external choice, then
         * there shouldn't be any acceptances.  Otherwise, the
         * Cartesian product of the acceptances from each process in
         * ‘Ps’ is used to get the acceptances for the external
         * choice.
         */

        if (!found_a_tau)
        {
            /*
             * Will store the Cartesian product.
             */

            alphabet_set_t  acceptances;

            /*
             * Start off by adding an empty alphabet to the product.
             */

            alphabet_t  empty;
            acceptances += empty;

            /*
             * Now, loop through each process in Ps.  For each, find
             * the Cartesian product of a) the process's acceptances,
             * and b) the Cartesian product from the previous
             * iteration.
             */

            for (stateset_t::iterator P_it = Ps.begin();
                 P_it != Ps.end();
                 ++P_it)
            {
                state_t  P = *P_it;
                alphabet_set_cp  P_alphas = _lts.get_acceptances(P);

                /*
                 * new_acceptances = *P_alphas × acceptances
                 */

                alphabet_set_t  new_acceptances;

                for (alphabet_set_t::iterator Pa_it = P_alphas->begin();
                     Pa_it != P_alphas->end();
                     ++Pa_it)
                {
                    for (alphabet_set_t::iterator ac_it =
                             acceptances.begin();
                         ac_it != acceptances.end();
                         ++ac_it)
                    {
                        alphabet_t  acceptance;

                        acceptance |= *Pa_it;
                        acceptance |= *ac_it;

                        new_acceptances += acceptance;
                    }
                }

                /*
                 * Replaced the old acceptances with the newly
                 * calculated one.
                 */

                acceptances.swap(new_acceptances);
            }

            /*
             * We have the acceptance set for the replicated
             * extchoice, so go ahead and add it to the LTS.
             */

            for (alphabet_set_t::iterator ac_it =
                     acceptances.begin();
                 ac_it != acceptances.end();
                 ++ac_it)
            {
                _lts.add_acceptance(dest, *ac_it);
            }
        }

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }

    state_t csp_t::add_replicated_extchoice(stateset_t &Ps)
    {
        ostringstream  key;
        state_t        dest;

        // Create the memoization key.
        key << "[]" << Ps;

        dest = lookup_memoized_process(key.str());
        if (dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            dest = add_temp_process();
            save_memoized_process(key.str(), dest);
            do_replicated_extchoice(*this, dest, Ps);
        }

        return dest;
    }

    void csp_t::replicated_extchoice(state_t dest, stateset_t &Ps)
    {
        ostringstream  key;
        state_t        old_dest;

        // Create the memoization key.
        key << "[]" << Ps;

        old_dest = lookup_memoized_process(key.str());
        if (old_dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            save_memoized_process(key.str(), dest);
            do_replicated_extchoice(*this, dest, Ps);
        } else {
            // We've already created this process, so let's just add a
            // single τ process to the previously calculated state.
            _lts.add_edge(dest, _tau, old_dest);
            _lts.finalize(dest);
        }
    }
}

#endif // HST_CSP_REPLICATED_EXTCHOICE_CC
