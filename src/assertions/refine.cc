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

#include <assert.h>
#include <deque>
#include <iostream>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_l.h>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>
#include <hst/assertions.hh>

using namespace std;

#ifndef DEBUG_REFINEMENT
#define DEBUG_REFINEMENT 0
#endif

namespace hst
{
    typedef pair<state_t, state_t>  check_pair_t;

    struct check_pair_t_hasher
    {
        unsigned long operator () (const check_pair_t &pair) const
        {
            return (pair.first * 33) + pair.second;
        }
    };

    typedef judy_set_l<check_pair_t, check_pair_t_hasher>
        check_pair_set_t;

    bool refines(const normalized_lts_t spec_norm, state_t spec_source,
                 const lts_t impl, state_t impl_source)
    {
        const lts_t  &spec = spec_norm.normalized();

        check_pair_set_t     seen;
        deque<check_pair_t>  pending;

        /*
         * Initialize the BFS sets with the source pair.
         */

        {
            check_pair_t  source(spec_source, impl_source);

            seen.insert(source);
            pending.push_back(source);
        }

        /*
         * Repeat for as long as we have more pairs to check
         */

        while (!pending.empty())
        {
            check_pair_t  pair = pending.front();
            pending.pop_front();

#if DEBUG_REFINEMENT
            cerr << "Checking (" << pair.first
                 << "," << pair.second << ")" << endl;
#endif

            /*
             * TRACES: Check that impl's set of initial events is a
             * subset of spec's (ignoring any τs in impl).
             */

            {
                alphabet_t  spec_initials
                    (spec.state_events_begin(pair.first),
                     spec.state_events_end(pair.first));

                alphabet_t  impl_initials
                    (impl.state_events_begin(pair.second),
                     impl.state_events_end(pair.second));

                impl_initials -= spec_norm.tau();

#if DEBUG_REFINEMENT
                cerr << "  " << spec_initials << " ?>= "
                     << impl_initials << endl;
#endif

                if (!(spec_initials >= impl_initials))
                {
#if DEBUG_REFINEMENT
                    cerr << "  Nope!  Refinement fails." << endl;
#endif
                    return false;
                }
            }

            /*
             * Add new pairs to the BFS queue based on spec's and
             * impl's outgoing edges.
             */

            for (lts_t::state_pairs_iterator sp_it =
                     impl.state_pairs_begin(pair.second);
                 sp_it != impl.state_pairs_end(pair.second);
                 ++sp_it)
            {
                event_t  event = sp_it->first;
                state_t  impl_prime = sp_it->second;

                if (event == spec_norm.tau())
                {
                    /*
                     * If the impl event is a τ, then the spec state
                     * stays the same, since the normalized spec
                     * includes a τ-closure.
                     */

                    check_pair_t  next(pair.first, impl_prime);

                    if (seen.find(next) == seen.end())
                    {
                        seen.insert(next);
                        pending.push_back(next);
                    }
                } else {
                    /*
                     * If it's not a τ, then there should be exactly
                     * one outgoing edge in spec for this event.
                     * (There can't be more than one since it's
                     * normalized, and there should be at least one
                     * since this impl state was part of the
                     * normalization.)
                     */

                    lts_t::event_target_iterator  et_it =
                        spec.event_targets_begin(pair.first, event);

                    if (et_it == spec.event_targets_end(pair.first,
                                                        event))
                    {
#if DEBUG_REFINEMENT
                        cerr << "ERROR: Spec and impl aren't consistent!"
                             << endl;
#endif
                        return false;
                    }

                    state_t  spec_prime = *et_it;
                    ++et_it;

                    if (et_it != spec.event_targets_end(pair.first,
                                                        event))
                    {
#if DEBUG_REFINEMENT
                        cerr << "ERROR: Spec and impl aren't consistent!"
                             << endl;
#endif
                        return false;
                    }

                    check_pair_t  next(spec_prime, impl_prime);

                    if (seen.find(next) == seen.end())
                    {
                        seen.insert(next);
                        pending.push_back(next);
                    }
                }
            }

        }

        /*
         * We've checked every reachable pair, so the refinement
         * succeeds.
         */

        return true;
    }
}

#endif // HST_CSP_INTERLEAVE_CC
