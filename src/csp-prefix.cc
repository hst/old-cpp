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

#ifndef HST_CSP_PREFIX_CC
#define HST_CSP_PREFIX_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>
#include <hst/csp-macros.hh>

using namespace std;

namespace hst
{
    void csp_t::prefix(state_t dest,
                       event_t a, state_t P)
    {
#if HST_CSP_DEBUG
        cerr << "Prefix " << dest
             << " = " << a << " -> " << P << endl;
#endif

        /*
         * ‘dest’ shouldn't be finalized, since this implies that it
         * already represents a different process.  ‘P’ does *not*
         * need to be finalized, since we don't have to know its
         * initial events to create [a→P].
         */

        REQUIRE_NOT_FINALIZED(dest);

        /*
         * A prefix process can only perform event ‘a’, after which is
         * behaves like ‘P’.
         */

        _lts.add_edge(dest, a, P);
        _lts.finalize(dest);
    }
}

#endif // HST_CSP_PREFIX_CC
