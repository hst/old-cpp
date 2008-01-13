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

#ifndef LTS_CC
#define LTS_CC

#include <hst/lts.hh>
#include <hst/io.hh>
#include <hst/io-macros.hh>

using namespace std;

namespace hst
{
    inline
    lts_t::graph_inner_map_p lts_t::graph_deref1(state_t state)
    {
        graph_inner_map_p  inner_map;
        pair<graph_t::iterator, bool>  insert_result;

#if HST_LTS_DEBUG
        cerr << "Deref1 ("
             << state << ")"
             << endl;
#endif

        // Try to insert a NULL inner map into the graph.  This
        // will tell us whether there's already an inner map for
        // this state.

        insert_result =
            graph.insert(make_pair(state, inner_map));

        if (insert_result.second)
        {
            // The insert succeeded, so there's currently a NULL
            // pointer in the outer map for this state.  We need
            // to change this NULL to a pointer to an actual inner
            // map; luckily, we've got an iterator we can use to
            // do this, though it's a bit tricky.

#if HST_LTS_DEBUG
            cerr << "  new = ";
#endif
            insert_result.first->second.reset(new graph_inner_map_t);
        } else {
            // The insert failed because there's already an inner
            // map.  Moreover, the iterator points to the inner
            // map's smart_ptr, so there's not really anything to
            // do here.

#if HST_LTS_DEBUG
            cerr << "  old = ";
#endif
        }

#if HST_LTS_DEBUG
        cerr << insert_result.first->second.get() << endl;
#endif
        return insert_result.first->second;
    }

    inline
    stateset_p lts_t::graph_deref2(state_t state, event_t event)
    {
        // Dereference the state first to get an inner map.

        graph_inner_map_p  inner_map = graph_deref1(state);

        // Right, now we perform basically the same logic as
        // graph_deref1.

        stateset_p  stateset;
        pair<graph_inner_map_t::iterator, bool>  insert_result;

#if HST_LTS_DEBUG
        cerr << "Deref2 ("
             << state << ","
             << event << ")"
             << endl
             << "  inner map = "
             << inner_map.get()
             << endl;
#endif

        // Try to insert a NULL stateset into the inner map.  This
        // will tell us whether there's already a stateset for
        // this (state, event) pair.

        insert_result =
            inner_map->insert(make_pair(event, stateset));

        if (insert_result.second)
        {
            // The insert succeeded, so there's currently a NULL
            // pointer in the inner map for this (state, event).
            // We need to change this NULL to a pointer to an
            // actual stateset; luckily, we've got an iterator we
            // can use to do this, though it's a bit tricky.

#if HST_LTS_DEBUG
            cerr << "  new = ";
#endif
            insert_result.first->second.reset(new stateset_t);
        } else {
            // The insert failed because there's already a
            // stateset.  Moreover, the iterator points to the
            // stateset's smart_ptr, so there's not really
            // anything to do here.

#if HST_LTS_DEBUG
            cerr << "  old = ";
#endif
        }

#if HST_LTS_DEBUG
        cerr << insert_result.first->second.get() << endl;
#endif
        return insert_result.first->second;
    }

    inline
    lts_t::graph_inner_map_p lts_t::graph_deref1(state_t state) const
    {
#if HST_LTS_DEBUG
        cerr << "Deref1 const ("
             << state << ")"
             << endl;
#endif

        graph_t::const_iterator  it = graph.find(state);

        if (it == graph.end())
        {
#if HST_LTS_DEBUG
            cerr << "  new" << endl;
#endif
            return graph_inner_map_p();
        } else {
#if HST_LTS_DEBUG
            cerr << "  old = " << it->second << endl;
#endif
            return graph_inner_map_p(it->second);
        }
    }

    inline
    stateset_p lts_t::graph_deref2(state_t state, event_t event) const
    {
#if HST_LTS_DEBUG
        cerr << "Deref2 const ("
             << state << ","
             << event << ")"
             << endl;
#endif

        // Dereference the state first to get an inner map.

        graph_inner_map_p  inner_map = graph_deref1(state);

#if HST_LTS_DEBUG
        cerr << "  inner map = "
             << inner_map.get()
             << endl;
#endif

        if (inner_map.get() == NULL)
        {
#if HST_LTS_DEBUG
            cerr << "  new outer" << endl;
#endif
            return stateset_p();
        }

        graph_inner_map_t::const_iterator  it =
            inner_map->find(event);

        if (it == inner_map->end())
        {
#if HST_LTS_DEBUG
            cerr << "  new inner" << endl;
#endif
            return stateset_p();
        } else {
#if HST_LTS_DEBUG
            cerr << "  old = " << it->second << endl;
#endif
            return stateset_p(it->second);
        }
    }

    void lts_t::add_edge(const state_t from,
                         const event_t event,
                         const state_t to)
    {
#if HST_LTS_DEBUG
        cerr << "Adding edge ("
             << from << "," << event << "," << to << ")" << endl;
#endif

        // Ensure that the state and event counters encompass these
        // states and events.

        if (from >= num_states)
            num_states = from + 1;

        if (event >= num_events)
            num_events = event + 1;

        if (to >= num_states)
            num_states = to + 1;

        // Now insert the «to» state into the stateset.

        stateset_p  stateset = graph_deref2(from, event);
        *stateset += to;
    }

    istream &operator >> (istream &stream, lts_t &lts)
    {
        lts_t    result;
        state_t  from, to;
        event_t  event;

        // First try to read the opening curly brace.

        require_char(stream, '{', true);
        PROPAGATE_ANY_ERROR(stream);

        // For the first element, we can either match a closing curly
        // brace, signifying the empty LTS, or we can match an LTS
        // link.

        require_char(stream, '}', true);
        EOF_IS_ERROR;
        PROPAGATE_IO_ERROR(stream);

        if (!stream.fail())
        {
            lts.swap(result);
            return stream;
        } else {
            stream.clear();
        }

        // It wasn't a brace, so it should be an LTS link.

        read_lts_link(stream, from, event, to, true);
        EOF_IS_ERROR;
        PROPAGATE_ANY_ERROR(stream);
        result.add_edge(from, event, to);

        // For the remaining elements, we must either match a closing
        // curly brace, or a comma followed by an LTS link.  Anything
        // else is a parse error.

        while (true)
        {
            require_char(stream, '}', true);
            EOF_IS_ERROR;
            PROPAGATE_IO_ERROR(stream);

            if (!stream.fail())
            {
                lts.swap(result);
                return stream;
            } else {
                stream.clear();
            }

            // It wasn't a brace, so it should be a comma followed by
            // an LTS link.

            require_char(stream, ',', true);
            EOF_IS_ERROR;
            PROPAGATE_ANY_ERROR(stream);

            read_lts_link(stream, from, event, to, true);
            EOF_IS_ERROR;
            PROPAGATE_ANY_ERROR(stream);
            result.add_edge(from, event, to);
        }
    }

    ostream &operator << (ostream &stream, const lts_t &lts)
    {
        lts_t::from_state_iterator    fs_it, fs_end;
        lts_t::state_events_iterator  se_it, se_end;
        lts_t::event_target_iterator  et_it, et_end;

        bool  first = true;

        stream << "{";
        fs_end = lts.from_states_end();
        for (fs_it = lts.from_states_begin();
             fs_it != fs_end; ++fs_it)
        {
            state_t  from_state = *fs_it;
            se_end = lts.state_events_end(from_state);

            for (se_it = lts.state_events_begin(from_state);
                 se_it != se_end; ++se_it)
            {
                event_t  event = *se_it;
                et_end = lts.event_targets_end(from_state, event);

                for (et_it = lts.event_targets_begin(from_state, event);
                     et_it != et_end; ++et_it)
                {
                    state_t  to_state = *et_it;

                    if (first)
                        first = false;
                    else
                        stream << ",";

                    stream << from_state << "--"
                           << event << "-->"
                           << to_state;
                }
            }
        }

        stream << "}";

        return stream;
    }
}

#endif // LTS_CC
