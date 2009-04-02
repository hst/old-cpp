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

#ifndef HST_CSP_REPLICATED_INTCHOICE_CC
#define HST_CSP_REPLICATED_INTCHOICE_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    static
    void do_replicated_intchoice(csp_t &csp, state_t dest,
                                 const stateset_t &Ps)
    {
        lts_t  &_lts = *csp.lts();

#if HST_CSP_DEBUG
        cerr << "Replicated intchoice " << dest
             << " = |~| " << Ps << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  We don't care
         * whether the states in Ps are finalized or not, but we do
         * require that there's at least one state in the set.
         */

        REQUIRE_NOT_FINALIZED(dest);

        if (Ps.size() <= 0)
        {
            cerr << "Replicated internal choice's operator cannot be empty."
                 << endl;

            return;
        }

        /*
         * A replicated internal choice can follow a τ event to any of
         * its branches.
         */

        for (stateset_t::iterator P_it = Ps.begin();
             P_it != Ps.end();
             ++P_it)
        {
            state_t  P = *P_it;
            _lts.add_edge(dest, csp.tau(), P);
        }

        /*
         * There are no acceptances for replicated internal choice,
         * since there are τ events.
         */

        /*
         * Lastly, finalize the ‘dest’ process.
         */

        _lts.finalize(dest);
    }

    state_t csp_t::add_replicated_intchoice(stateset_t &Ps)
    {
        ostringstream  key;
        state_t        dest;

        // Create the memoization key.
        key << "|~|" << Ps;

        dest = lookup_memoized_process(key.str());
        if (dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            dest = add_temp_process();
            save_memoized_process(key.str(), dest);
            do_replicated_intchoice(*this, dest, Ps);
        }

        return dest;
    }

    void csp_t::replicated_intchoice(state_t dest, stateset_t &Ps)
    {
        ostringstream  key;
        state_t        old_dest;

        // Create the memoization key.
        key << "|~|" << Ps;

        old_dest = lookup_memoized_process(key.str());
        if (old_dest == HST_ERROR_STATE)
        {
            // We haven't created this process yet, so do so.
            save_memoized_process(key.str(), dest);
            do_replicated_intchoice(*this, dest, Ps);
        } else {
            // We've already created this process, so let's just add a
            // single τ process to the previously calculated state.
            _lts.add_edge(dest, _tau, old_dest);
            _lts.finalize(dest);
        }
    }
}

#endif // HST_CSP_REPLICATED_INTCHOICE_CC
