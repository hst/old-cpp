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

#include <iostream>

#include <hst/lts.hh>
#include <hst/normalized-lts.hh>
#include <hst/csp.hh>
#include <hst/assertions.hh>

using namespace std;
using namespace hst;

int main()
{
    csp_t  csp;

    read_csp0(cin, csp);

    if (!cin.fail())
    {
        state_t  SPEC;

        {
            // Read the SPEC process and normalize it.

            SPEC = csp.get_process("SPEC");
            if (SPEC == HST_ERROR_STATE)
                return 1;

            csp.normalized_lts()->prenormalize(SPEC);
            csp.normalized_lts()->normalize();

            SPEC = csp.normalized_lts()->initial_normal_state(SPEC);
        }

        // Read the IMPL process.

        state_t  IMPL;

        IMPL = csp.get_process("IMPL");
        if (IMPL == HST_ERROR_STATE)
            return 2;

        // Check the refinement

        bool  result = refines(*csp.normalized_lts(), SPEC,
                               *csp.lts(), IMPL);

        cout << result << endl;
    }
}
