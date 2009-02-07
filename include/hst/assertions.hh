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

#ifndef HST_ASSERTIONS_HH
#define HST_ASSERTIONS_HH

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>

namespace hst
{
    struct trace_counterexample_t
    {
        trace_t  trace;
        event_t  event;
        state_t  spec_state;
        state_t  impl_state;
    };

    struct failures_counterexample_t
    {
        trace_t     trace;
        alphabet_t  acceptance;
        state_t     spec_state;
        state_t     impl_state;
    };

    bool trace_refines(trace_counterexample_t &counter,
                       const normalized_lts_t &spec, state_t spec_source,
                       const lts_t &impl, state_t impl_source);

    bool failures_refines(failures_counterexample_t &counter,
                          const normalized_lts_t &spec, state_t spec_source,
                          const lts_t &impl, state_t impl_source);
}

#endif // HST_ASSERTIONS_H
