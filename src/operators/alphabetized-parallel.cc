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

#ifndef HST_CSP_ALPHABETIZED_PARALLEL_CC
#define HST_CSP_ALPHABETIZED_PARALLEL_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/csp.hh>

using namespace std;

namespace hst
{
    /*
     * We can implement alphabetized parallel in terms of interface
     * parallel — just intersect the two process alphabets to get the
     * synchronization alphabet.
     */

    state_t csp_t::add_alphabetized_parallel
    (state_t P, alphabet_t &alphaP, alphabet_t &alphaQ, state_t Q)
    {
        alphabet_t  alpha = alphaP;
        alpha &= alphaQ;

        return add_interface_parallel(P, alpha, Q);
    }

    void csp_t::alphabetized_parallel
    (state_t dest,
     state_t P, alphabet_t &alphaP, alphabet_t &alphaQ, state_t Q)
    {
#if HST_CSP_DEBUG
        cerr << "Alphabetized parallel:" << endl;
#endif

        alphabet_t  alpha = alphaP;
        alpha &= alphaQ;


        interface_parallel(dest, P, alpha, Q);
    }
}

#endif // HST_CSP_ALPHABETIZED_PARALLEL_CC
