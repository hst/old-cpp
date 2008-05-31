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
#include <boost/iterator/filter_iterator.hpp>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_l.h>

#include <hst/types.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>
#include <hst/assertions.hh>

using namespace std;
using namespace boost;

#ifndef DEBUG_REFINEMENT
#define DEBUG_REFINEMENT 0
#endif

namespace hst
{
    struct check_pair_t;
    typedef shared_ptr<check_pair_t>        check_pair_p;
    typedef shared_ptr<const check_pair_t>  check_pair_cp;

    struct check_pair_t
    {
        state_t  spec;
        state_t  impl;
        event_t  inbound_event;
        check_pair_cp  parent;

        check_pair_t(state_t _spec, state_t _impl):
            spec(_spec),
            impl(_impl),
            inbound_event(HST_ERROR_EVENT)
        {
        }

        check_pair_t(state_t _spec, state_t _impl,
                     event_t _inbound_event,
                     check_pair_cp _parent):
            spec(_spec),
            impl(_impl),
            inbound_event(_inbound_event),
            parent(_parent)
        {
        }

        bool operator == (const check_pair_t &other) const
        {
            return (spec == other.spec) && (impl == other.impl);
        }
    };

    struct check_pair_t_hasher
    {
        unsigned long operator () (const check_pair_t &pair) const
        {
            return (pair.spec * 33) + pair.impl;
        }

        unsigned long operator () (check_pair_cp pair) const
        {
            return (pair->spec * 33) + pair->impl;
        }

    };

    static
    void construct_counterexample(trace_counterexample_t &counter,
                                  event_t event,
                                  check_pair_cp pair)
    {
        trace_counterexample_t  result;

        for (check_pair_cp  current = pair;
             current->parent.get() != NULL;
             current = current->parent)
        {
            result.trace.push_front(current->inbound_event);
        }

        result.event = event;
        result.spec_state = pair->spec;
        result.impl_state = pair->impl;

        std::swap(counter,result);
    }

    typedef judy_set_l<check_pair_t, check_pair_t_hasher>
        check_pair_set_t;

    bool refines(trace_counterexample_t &counter,
                 const normalized_lts_t spec_norm, state_t spec_source,
                 const lts_t impl, state_t impl_source)
    {
        const lts_t  &spec = spec_norm.normalized();

        check_pair_set_t      seen;
        deque<check_pair_cp>  pending;

        /*
         * Initialize the BFS sets with the source pair.
         */

        {
            check_pair_cp  source
                (new check_pair_t(spec_source, impl_source));

            seen.insert(*source);
            pending.push_back(source);
        }

        /*
         * Repeat for as long as we have more pairs to check
         */

        while (!pending.empty())
        {
            check_pair_cp  pair = pending.front();
            pending.pop_front();

#if DEBUG_REFINEMENT
            cerr << "Checking (" << pair->spec
                 << "," << pair->impl << ")" << endl;
#endif

            /*
             * TRACES: Check that impl's set of initial events is a
             * subset of spec's (ignoring any τs in impl).
             */

            {
#if DEBUG_REFINEMENT
                {
                    alphabet_t  spec_initials
                        (spec.state_events_begin(pair->spec),
                         spec.state_events_end(pair->spec));

                    alphabet_t  impl_initials
                        (impl.state_events_begin(pair->impl),
                         impl.state_events_end(pair->impl));

                    cerr << "  " << spec_initials << " ?>= "
                         << impl_initials << endl;
                }
#endif

                /*
                 * We need to skip over any τ events when we look
                 * through IMPL's initials set.
                 */

                skip_taus  skipper(spec_norm.tau());

                typedef filter_iterator
                    <skip_taus, lts_t::state_events_iterator>
                    se_skip_iterator;

                se_skip_iterator  impl_begin
                    (skipper,
                     impl.state_events_begin(pair->impl),
                     impl.state_events_end(pair->impl));

                se_skip_iterator  impl_end;

                /*
                 * See if SPEC ⊇ IMPL, skipping over any τs in IMPL.
                 */

                std::pair<bool, unsigned long>  proof =
                    is_superset_with_proof
                    (spec.state_events_begin(pair->spec),
                     spec.state_events_end(pair->spec),
                     impl_begin,
                     impl_end);

                /*
                 * If SPEC ⊉ IMPL, then the refinement fails.
                 */

                if (!proof.first)
                {
#if DEBUG_REFINEMENT
                    cerr << "  Nope!  Refinement fails." << endl;
#endif

                    /*
                     * We can use any of the events left in the set as
                     * the counterexample event.
                     */

                    construct_counterexample(counter, proof.second, pair);
                    return false;
                }
            }

            /*
             * Add new pairs to the BFS queue based on spec's and
             * impl's outgoing edges.
             */

            for (lts_t::state_pairs_iterator sp_it =
                     impl.state_pairs_begin(pair->impl);
                 sp_it != impl.state_pairs_end(pair->impl);
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

                    check_pair_cp  next
                        (new check_pair_t(pair->spec, impl_prime,
                                          spec_norm.tau(), pair));

                    if (seen.find(*next) == seen.end())
                    {
                        seen.insert(*next);
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
                        spec.event_targets_begin(pair->spec, event);

                    if (et_it == spec.event_targets_end(pair->spec,
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

                    if (et_it != spec.event_targets_end(pair->spec,
                                                        event))
                    {
#if DEBUG_REFINEMENT
                        cerr << "ERROR: Spec and impl aren't consistent!"
                             << endl;
#endif
                        return false;
                    }

                    check_pair_cp  next
                        (new check_pair_t(spec_prime, impl_prime,
                                          event, pair));

                    if (seen.find(*next) == seen.end())
                    {
                        seen.insert(*next);
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
