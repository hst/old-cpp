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

#ifndef HST_CSP_INTERLEAVE_CC
#define HST_CSP_INTERLEAVE_CC

#include <iostream>
#include <sstream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    /*
     * We can implement interleaving in terms of interface parallel —
     * just use an empty synchronization alphabet.
     */

    state_t csp_t::add_interleave(state_t P, state_t Q)
    {
        alphabet_t  alpha;
        return add_interface_parallel(P, alpha, Q);
    }

    void csp_t::interleave(state_t dest, state_t P, state_t Q)
    {
#if HST_CSP_DEBUG
        cerr << "Interleaving:" << endl;
#endif

        alphabet_t  alpha;
        interface_parallel(dest, P, alpha, Q);
    }
}

#endif // HST_CSP_INTERLEAVE_CC
