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

#ifndef HST_LTS_HH
#define HST_LTS_HH

#include <assert.h>
#include <functional>
#include <iostream>
#include <string>
#include <tr1/memory>
#include <boost/iterator/transform_iterator.hpp>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/types.hh>
#include <hst/equivalence.hh>
#include <hst/event-stateset-map.hh>

#ifndef HST_LTS_DEBUG
#define HST_LTS_DEBUG 0
#endif

using namespace std;

namespace hst
{
    struct lts_edge_t
    {
        state_t  from;
        event_t  event;
        state_t  to;

        lts_edge_t():
            from(HST_ERROR_STATE),
            event(HST_ERROR_EVENT),
            to(HST_ERROR_STATE)
        {
        }

        lts_edge_t(state_t _from, event_t _event, state_t _to):
            from(_from),
            event(_event),
            to(_to)
        {
        }
    };

    class lts_t
    {
    protected:
        /// The number of states in the LTS
        state_t  num_states;

        typedef judy_map_l<state_t, string,
                           state_t_hasher>    state_name_map_t;
        typedef shared_ptr<state_name_map_t>  state_name_map_p;

        /// The name of each LTS state
        state_name_map_t  state_names;

        /// The number of events in the LTS
        event_t  num_events;

        typedef judy_map_l<event_t, string,
                           event_t_hasher>    event_name_map_t;
        typedef shared_ptr<event_name_map_t>  event_name_map_p;

        /// The name of each LTS event
        event_name_map_t  event_names;

        /**
         * We keep track of which states have been “finalized”.
         * Before a state is finalized, you cannot rely on having an
         * accurate view of its outgoing edges; afterwards, you cannot
         * add any more outgoing edges to it.
         */

        stateset_t  finalized_states;

        /**
         * The LTS graph represented as a mapping between (state,
         * event) pairs and sets of events.
         */

        typedef judy_map_l<state_t, event_stateset_map_p,
                           state_t_hasher>                 graph_t;
        typedef shared_ptr<graph_t>                        graph_p;

        graph_t  graph;

        /**
         * Dereference the LTS graph by a from_state, creating the
         * corresponding inner_map if necessary.
         */

        event_stateset_map_p graph_deref1(state_t state);

        /**
         * Dereference the LTS graph by a from_state and an event,
         * creating the corresponding inner_map and stateset if
         * necessary.
         */

        stateset_p graph_deref2(state_t state, event_t event);

        /**
         * Dereference the LTS graph by a from_state, returning a NULL
         * pointer if the inner_map doesn't exist.
         */

        event_stateset_map_cp graph_deref1(state_t state) const;

        /**
         * Dereference the LTS graph by a from_state and an event,
         * returning a NULL pointer if the corresponding inner_map or
         * stateset doesn't exit.
         */

        stateset_cp graph_deref2(state_t state, event_t event) const;

        /*
         * The refusals of an LTS are stored as minimal acceptance
         * sets.
         */

        typedef judy_map_l<state_t, alphabet_set_p,
                           state_t_hasher>           acceptances_t;
        typedef shared_ptr<acceptances_t>            acceptances_p;

        acceptances_t  acceptances;

    public:
        void add_edge(const state_t from,
                      const event_t event,
                      const state_t to);

        void add_edge(const lts_edge_t &edge)
        {
            add_edge(edge.from, edge.event, edge.to);
        }

        void add_acceptance(const state_t state,
                            const alphabet_t &alphabet);

        alphabet_set_cp get_acceptances(const state_t state) const
        {
            acceptances_t::const_iterator  it =
                acceptances.find(state);

            if (it == acceptances.end())
            {
                // There aren't any acceptances for this state yet, so
                // return an empty (but not NULL) alphabet set.

                alphabet_set_cp  alphabet_set(new alphabet_set_t);
                return alphabet_set;
            } else {
                return it->second;
            }
        }

        void closure
        (event_t event, stateset_t &closure,
         const stateset_t &initial) const;

        void divergent_nodes
        (event_t event, stateset_t &divergent) const;

        void bisimulate(equivalences_t &equiv,
                        semantic_model_t semantic_model) const;

        lts_t():
            num_states(0L),
            num_events(0L)
        {
        }

        lts_t(const lts_t &other):
            num_states(other.num_states),
            state_names(other.state_names),
            num_events(other.num_events),
            event_names(other.event_names),
            finalized_states(other.finalized_states),
            graph(other.graph),
            acceptances(other.acceptances)
        {
        }

        void clear()
        {
            num_states = 0;
            num_events = 0;
            state_names.clear();
            event_names.clear();
            finalized_states.clear();
            graph.clear();
            acceptances.clear();
        }

        void swap(lts_t &other)
        {
            std::swap(num_states, other.num_states);
            state_names.swap(other.state_names);
            std::swap(num_events, other.num_events);
            event_names.swap(other.event_names);
            finalized_states.swap(other.finalized_states);            
            graph.swap(other.graph);
            acceptances.swap(other.acceptances);
        }

        lts_t &operator = (const lts_t &other)
        {
            if (this != &other)
            {
                clear();
                lts_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        state_t add_state(string name)
        {
            state_t  state = num_states++;
            state_names.insert(make_pair(state, name));
            return state;
        }

        unsigned long state_count() const
        {
            return num_states;
        }

        string get_state_name(state_t state) const
        {
            state_name_map_t::const_iterator  it =
                state_names.find(state);

            if (it == state_names.end())
            {
                return "unknown state";
            } else {
                return it->second;
            }
        }

        event_t add_event(string name)
        {
            event_t  event = num_events++;
            event_names.insert(make_pair(event, name));
            return event;
        }

        unsigned long event_count() const
        {
            return num_events;
        }

        string get_event_name(event_t event) const
        {
            event_name_map_t::const_iterator  it =
                event_names.find(event);

            if (it == event_names.end())
            {
                return "unknown event";
            } else {
                return it->second;
            }
        }

        bool is_finalized(state_t state) const
        {
            return finalized_states.contains(state);
        }

        void finalize(state_t state)
        {
            finalized_states += state;
        }

    protected:
        /*
         * The underlying iterator returns a (state_t,
         * graph_inner_map_p) pair, so we can get the state by taking
         * the first element of the pair.
         */

        struct from_state_evaluator:
            public unary_function
            <graph_t::const_iterator::const_reference, state_t>
        {
            result_type operator () (argument_type it) const
            {
                return it.first;
            }
        };

    public:
        typedef boost::transform_iterator
        <from_state_evaluator, graph_t::const_iterator>
            from_state_iterator;

        from_state_iterator from_states_begin() const
        {
            graph_t::const_iterator  it = graph.begin();
            return from_state_iterator(it);
        }

        from_state_iterator from_states_end() const
        {
            graph_t::const_iterator  it = graph.end();
            return from_state_iterator(it);
        }

        typedef event_stateset_map_t::events_iterator
            state_events_iterator;

        state_events_iterator state_events_begin(state_t from) const
        {
            event_stateset_map_cp  inner_map = graph_deref1(from);
            if (inner_map.get() == NULL)
            {
                return state_events_iterator();
            } else {
                return inner_map->events_begin();
            }
        }

        state_events_iterator state_events_end(state_t from) const
        {
            event_stateset_map_cp  inner_map = graph_deref1(from);
            if (inner_map.get() == NULL)
            {
                return state_events_iterator();
            } else {
                return inner_map->events_end();
            }
        }

        typedef stateset_t::iterator  event_target_iterator;

        event_target_iterator event_targets_begin(state_t from,
                                                  event_t event) const
        {
            stateset_cp  stateset = graph_deref2(from, event);

            if (stateset.get() == NULL)
            {
                return event_target_iterator();
            } else {
                return stateset->begin();
            }
        }

        event_target_iterator event_targets_end(state_t from,
                                                event_t event) const
        {
            stateset_cp  stateset = graph_deref2(from, event);

            if (stateset.get() == NULL)
            {
                return event_target_iterator();
            } else {
                return stateset->end();
            }
        }

        typedef event_stateset_map_t::pairs_iterator
            state_pairs_iterator;

        state_pairs_iterator state_pairs_begin(state_t from) const
        {
            event_stateset_map_cp  inner_map = graph_deref1(from);
            if (inner_map.get() == NULL)
            {
                return state_pairs_iterator();
            } else {
                return inner_map->pairs_begin();
            }
        }

        state_pairs_iterator state_pairs_end(state_t from) const
        {
            event_stateset_map_cp  inner_map = graph_deref1(from);
            if (inner_map.get() == NULL)
            {
                return state_pairs_iterator();
            } else {
                return inner_map->pairs_end();
            }
        }

        typedef acceptances_t::const_iterator  acceptances_iterator;

        acceptances_iterator acceptances_begin() const
        {
            return acceptances.begin();
        }

        acceptances_iterator acceptances_end() const
        {
            return acceptances.end();
        }
    };

    typedef shared_ptr<lts_t>        lts_p;
    typedef shared_ptr<const lts_t>  lts_cp;

    // Input and output operators for streams
    istream &operator >> (istream &stream, lts_t &lts);
    ostream &operator << (ostream &stream, const lts_t &lts);
}

#endif // HST_LTS_H
