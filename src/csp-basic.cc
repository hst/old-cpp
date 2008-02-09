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

#ifndef HST_CSP_BASIC_CC
#define HST_CSP_BASIC_CC

#include <iostream>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/csp.hh>

using namespace std;

namespace hst
{
    ostream &operator << (ostream &stream, const csp_t &csp)
    {
        unsigned long  i;
        bool  first;

        // Output the process names

        stream << "processes {";
        first = true;

        for (i = 0; i < csp.lts()->state_count(); i++)
        {
            if (first)
                first = false;
            else
                stream << ",";

            stream << i << ":" << csp.lts()->get_state_name(i);
        }

        stream << "}" << endl;

        // Output the event names

        stream << "events {";
        first = true;

        for (i = 0; i < csp.lts()->event_count(); i++)
        {
            if (first)
                first = false;
            else
                stream << ",";

            stream << i << ":" << csp.lts()->get_event_name(i);
        }

        stream << "}" << endl;

        // Output the LTS

        stream << *csp.lts() << endl;

        return stream;
    }
}

#endif // HST_CSP_BASIC_CC
