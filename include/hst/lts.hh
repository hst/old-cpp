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

#ifndef HST_LTS_HH
#define HST_LTS_HH

#include <assert.h>
#include <iostream>
#include <string>
#include <tr1/memory>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/types.hh>

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

        /// The name of each LTS state
        judy_map_l<state_t, string, state_t_hasher>  state_names;

        /// The number of events in the LTS
        event_t  num_events;

        /// The name of each LTS event
        judy_map_l<event_t, string, event_t_hasher>  event_names;

        /**
         * The LTS graph represented as a mapping between (state,
         * event) pairs and sets of events.
         */

        typedef judy_map_l<event_t, stateset_p,
                           event_t_hasher>       graph_inner_map_t;
        typedef shared_ptr<graph_inner_map_t>    graph_inner_map_p;

        typedef judy_map_l<state_t, graph_inner_map_p,
                           state_t_hasher>              graph_t;
        typedef shared_ptr<graph_t>                     graph_p;

        graph_t  graph;

        /**
         * Dereference the LTS graph by a from_state, creating the
         * corresponding inner_map if necessary.
         */

        graph_inner_map_p graph_deref1(state_t state);

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

        graph_inner_map_p graph_deref1(state_t state) const;

        /**
         * Dereference the LTS graph by a from_state and an event,
         * returning a NULL pointer if the corresponding inner_map or
         * stateset doesn't exit.
         */

        stateset_p graph_deref2(state_t state, event_t event) const;

        /*
         * The graph LTS iterators all have basically the same format,
         * so we'll define it once using a template.  They all use an
         * underlying iterator to perform all of the work, possibly
         * modifying the result before it's returned.  «Iterator» is
         * the base type of the underlying iterator.  «Result» is the
         * value return by this iterator.  «Eval» is a functor that
         * returns the current value from the underlying iterator.
         */

        template<typename Iterator,
                 typename Result,
                 typename Eval>
        class proxy_iterator
        {
        private:
            typedef proxy_iterator<Iterator, Result, Eval>
                this_type;

            Eval      eval;
            Iterator  it;

        public:
            proxy_iterator()
            {
            }

            proxy_iterator(Iterator &_it):
                it(_it)
            {
            }

            Result operator * ()
            {
                return eval(it);
            }

            this_type operator ++ (int)
            {
                this_type  ret = *this;
                ++it;
                return ret;
            }

            this_type &operator ++ ()
            {
                ++it;
                return *this;
            }

            bool operator == (const this_type &other)
            {
                return (this->it == other.it);
            }

            bool operator != (const this_type &other)
            {
                return (this->it != other.it);
            }
        };

        /*
         * The underlying iterator returns a (state_t,
         * graph_inner_map_p) pair, so we can get the state by taking
         * the first element of the pair.
         */

        struct from_state_evaluator
        {
            state_t operator () (graph_t::const_iterator &it)
            {
                return it->first;
            }
        };

        /*
         * The underlying iterator returns an (event_t, stateset_p)
         * pair, so we can get the event by taking the first element
         * of the pair.
         */

        struct state_events_evaluator
        {
            event_t operator () (graph_inner_map_t::const_iterator &it)
            {
                return it->first;
            }
        };

        /*
         * We only have to dereference the underlying iterator, since
         * it already returns a state.
         */

        struct event_target_evaluator
        {
            state_t operator () (stateset_t::iterator &it)
            {
                return *it;
            }
        };

    public:
        void add_edge(const state_t from,
                      const event_t event,
                      const state_t to);

        void add_edge(const lts_edge_t &edge)
        {
            add_edge(edge.from, edge.event, edge.to);
        }

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
            graph(other.graph)
        {
        }

        void clear()
        {
            num_states = 0;
            num_events = 0;
            state_names.clear();
            event_names.clear();
            graph.clear();
        }

        void swap(lts_t &other)
        {
            std::swap(num_states, other.num_states);
            state_names.swap(other.state_names);
            std::swap(num_events, other.num_events);
            event_names.swap(other.event_names);
            graph.swap(other.graph);
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

        event_t add_event(string name)
        {
            event_t  event = num_events++;
            event_names.insert(make_pair(event, name));
            return event;
        }

        typedef proxy_iterator<graph_t::const_iterator,
                               state_t,
                               from_state_evaluator>
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

        typedef proxy_iterator<graph_inner_map_t::const_iterator,
                               event_t,
                               state_events_evaluator>
            state_events_iterator;

        state_events_iterator state_events_begin(state_t from) const
        {
            graph_inner_map_p  inner_map = graph_deref1(from);
            graph_inner_map_t::const_iterator  it =
                inner_map->begin();
            return state_events_iterator(it);
        }

        state_events_iterator state_events_end(state_t from) const
        {
            graph_inner_map_p  inner_map = graph_deref1(from);
            graph_inner_map_t::const_iterator  it =
                inner_map->end();
            return state_events_iterator(it);
        }

        typedef proxy_iterator<stateset_t::iterator,
                               state_t,
                               event_target_evaluator>
            event_target_iterator;

        event_target_iterator event_targets_begin(state_t from,
                                                  event_t event) const
        {
            stateset_p  stateset = graph_deref2(from, event);
            stateset_t::iterator  it = stateset->begin();
            return event_target_iterator(it);
        }

        event_target_iterator event_targets_end(state_t from,
                                                event_t event) const
        {
            stateset_p  stateset = graph_deref2(from, event);
            stateset_t::iterator  it = stateset->end();
            return event_target_iterator(it);
        }

    };

    // Input and output operators for streams
    istream &operator >> (istream &stream, lts_t &lts);
    ostream &operator << (ostream &stream, const lts_t &lts);
}

#endif // HST_LTS_H
