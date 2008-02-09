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

#ifndef HST_NORMALIZED_LTS_HH
#define HST_NORMALIZED_LTS_HH

#include <tr1/memory>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_map_l.h>

#include <hst/types.hh>
#include <hst/lts.hh>

using namespace std;

namespace hst
{
    /**
     * A normalized LTS is similar to a standard LTS, with two major
     * differences:
     *
     *   1. Each node represents a /set/ of states, rather than a
     *      single state.
     *
     *   2. Each node can have at most one outgoing transition for
     *      each event.
     */

    class normalized_lts_t
    {
    protected:
        /**
         * The source LTS that this is a normalization of.
         */

        lts_t  &_source;

        /**
         * The τ event for this normalization.
         */
        event_t  tau;

        /**
         * We use an LTS to store the graph structure of the
         * normalized LTS.  Care should be taken, though: the “states”
         * in this LTS represent *sets* of states from the original
         * LTS; the mapping between the two is provided by the
         * [states] and [sets] maps.
         */

        lts_t  _normalized;

        /**
         * The mapping from sets of source LTS states to normalized
         * LTS states.
         */

        typedef judy_map_l<stateset_cp, state_t,
                           intset_t_hasher,
                           intset_t_hasher>       set_state_map_t;
        typedef shared_ptr<set_state_map_t>       set_state_map_p;

        set_state_map_t  states;

        /**
         * The inverse of [states] — a mapping from normalized LTS states
         * to the set of source LTS states that each represents.
         */

        typedef judy_map_l<state_t, stateset_cp,
                           state_t_hasher>        state_set_map_t;
        typedef shared_ptr<state_set_map_t>       state_set_map_p;

        state_set_map_t  sets;

        /**
         * A set of the normalized states that have been fully
         * prenormalized.
         */

        stateset_t  prenormalized;

    public:
        friend
        ostream &operator <<
        (ostream &stream, const normalized_lts_t &normalized);

        normalized_lts_t(lts_t &__source, event_t _tau):
            _source(__source),
            tau(_tau),
            _normalized(),
            states(),
            sets(),
            prenormalized()
        {
        }

        normalized_lts_t(const normalized_lts_t &other):
            _source(other._source),
            tau(other.tau),
            _normalized(other._normalized),
            states(other.states),
            sets(other.sets),
            prenormalized(other.prenormalized)
        {
        }

        void clear()
        {
            _normalized.clear();
            states.clear();
            sets.clear();
            prenormalized.clear();
        }

        void swap(normalized_lts_t &other)
        {
            _source.swap(other._source);
            std::swap(tau, other.tau);
            _normalized.swap(other._normalized);
            states.swap(other.states);
            sets.swap(other.sets);
            prenormalized.swap(other.prenormalized);
        }

        normalized_lts_t &operator = (const normalized_lts_t &other)
        {
            if (this != &other)
            {
                clear();
                normalized_lts_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        lts_t &source()
        {
            return _source;
        }

        const lts_t &source() const
        {
            return _source;
        }

        lts_t &normalized()
        {
            return _normalized;
        }

        const lts_t &normalized() const
        {
            return _normalized;
        }

        typedef lts_t::from_state_iterator  states_iterator;

        states_iterator states_begin() const
        {
            return _normalized.from_states_begin();
        }

        states_iterator states_end() const
        {
            return _normalized.from_states_end();
        }

        state_t get_normalized_state(stateset_cp set);
        stateset_cp get_normalized_set(state_t state) const;

        state_t prenormalize(state_t source_state);
    };

    ostream &operator <<
    (ostream &stream, const normalized_lts_t &normalized);
}

#endif // HST_NORMALIZED_LTS_H
