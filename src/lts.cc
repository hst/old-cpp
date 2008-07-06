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

#ifndef LTS_CC
#define LTS_CC

#include <hst/types.hh>
#include <hst/event-stateset-map.hh>
#include <hst/lts.hh>

using namespace std;

namespace hst
{
    event_stateset_map_p lts_t::graph_deref1(state_t state)
    {
        event_stateset_map_p  inner_map;
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
            insert_result.first->second.reset(new event_stateset_map_t);
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

    stateset_p lts_t::graph_deref2(state_t state, event_t event)
    {
        // Dereference the state first to get an inner map.

        event_stateset_map_p  inner_map = graph_deref1(state);
        return inner_map->get(event);
    }

    event_stateset_map_cp lts_t::graph_deref1(state_t state) const
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
            return event_stateset_map_cp();
        } else {
#if HST_LTS_DEBUG
            cerr << "  old = " << it->second << endl;
#endif
            return event_stateset_map_cp(it->second);
        }
    }

    stateset_cp lts_t::graph_deref2(state_t state, event_t event) const
    {
#if HST_LTS_DEBUG
        cerr << "Deref2 const ("
             << state << ","
             << event << ")"
             << endl;
#endif

        // Dereference the state first to get an inner map.

        event_stateset_map_cp  inner_map = graph_deref1(state);

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
        } else {
            stateset_cp  result = inner_map->get(event);

#if HST_LTS_DEBUG
            cerr << "  old " << result << endl;
#endif

            return result;
        }
    }

    void lts_t::add_edge(const state_t from,
                         const event_t event,
                         const state_t to)
    {
        if (is_finalized(from))
            return;

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

    void lts_t::add_acceptance(const state_t state,
                               const alphabet_t &alphabet)
    {
        if (is_finalized(state))
            return;

#if HST_LTS_DEBUG
        cerr << "Adding acceptance to "
             << state << ": " << alphabet << endl;
#endif

        alphabet_set_p  alphabet_set;
        pair<acceptances_t::iterator, bool>  insert_result;

        // Try to insert a NULL alphabet set.  This will tell us
        // whether there's already an alphabet set for this state.

        insert_result =
            acceptances.insert(make_pair(state, alphabet_set));

        if (insert_result.second)
        {
            // The insert succeeded, so there's currently a NULL
            // pointer in the acceptance map for this state.  We need
            // to change this NULL to a pointer to an actual alphabet
            // set; luckily, we've got an iterator we can use to do
            // this, though it's a bit tricky.

            insert_result.first->second.reset(new alphabet_set_t);
        } else {
            // The insert failed because there's already an alphabet
            // set.  Moreover, the iterator points to the alphabet
            // set's smart_ptr, so there's not really anything to do
            // here.
        }

        // Now we can add the alphabet to this alphabet set.
        *(insert_result.first->second) += alphabet;
    }

    ostream &operator << (ostream &stream, const lts_t &lts)
    {
        lts_t::from_state_iterator   fs_it, fs_end;
        lts_t::state_pairs_iterator  sp_it, sp_end;

        bool  first = true;

        stream << "edges {";

        fs_end = lts.from_states_end();
        for (fs_it = lts.from_states_begin();
             fs_it != fs_end; ++fs_it)
        {
            state_t  from_state = *fs_it;
            sp_end = lts.state_pairs_end(from_state);

            for (sp_it = lts.state_pairs_begin(from_state);
                 sp_it != sp_end; ++sp_it)
            {
                event_t  event    = sp_it->first;
                state_t  to_state = sp_it->second;

                if (first)
                    first = false;
                else
                    stream << ", ";

                stream << from_state << "--"
                       << event << "-->"
                       << to_state;
            }
        }

        stream << "}" << endl << "acceptances {";

        first = true;

        fs_end = lts.from_states_end();
        for (state_t  state = 0;
             state < lts.state_count();
             ++state)
        {
            if (first)
                first = false;
            else
                stream << ", ";

            stream << state << ":"
                   << *lts.get_acceptances(state);
        }

        stream << "}" << endl;

        return stream;
    }
}

#endif // LTS_CC
