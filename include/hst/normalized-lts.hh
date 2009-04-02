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

#ifndef HST_NORMALIZED_LTS_HH
#define HST_NORMALIZED_LTS_HH

#include <boost/shared_ptr.hpp>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_map_l.h>

#include <hst/types.hh>
#include <hst/lts.hh>

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
         * There are two stages that a normalized LTS can be in.
         * After it's first created, you can prenormalize as many
         * source states as you want.  The first stage is ended by
         * calling the normalize() function, which completes the
         * normalization.  After this point, the normalized LTS is
         * effectively “locked down”, and no useful additions can be
         * made to it.
         */

        enum stage_t
        {
            PRENORMALIZING,
            NORMALIZED
        } _stage;

        /**
         * The semantic model used for this normalized LTS.
         */

        semantic_model_t  _semantic_model;

        /**
         * The source LTS that this is a normalization of.
         */

        lts_t  *_source;

        /**
         * The τ event for this normalization.
         */
        event_t  _tau;

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
                           intset_t_hasher>         set_state_map_t;
        typedef boost::shared_ptr<set_state_map_t>  set_state_map_p;

        set_state_map_t  states;

        /**
         * The inverse of [states] — a mapping from normalized LTS states
         * to the set of source LTS states that each represents.
         */

        typedef judy_map_l<state_t, stateset_cp,
                           state_t_hasher>          state_set_map_t;
        typedef boost::shared_ptr<state_set_map_t>  state_set_map_p;

        state_set_map_t  sets;

        /**
         * A mapping from initial source sets to initial normalized
         * sets.
         */

        typedef judy_map_l<state_t, state_t,
                           state_t_hasher>        initial_map_t;
        typedef boost::shared_ptr<initial_map_t>  initial_map_p;

        initial_map_t  initial_map;

        /**
         * A set of the normalized states that have been fully
         * prenormalized.
         */

        stateset_t  prenormalized;

    public:
        friend
        std::ostream &operator <<
        (std::ostream &stream, const normalized_lts_t &normalized);

        normalized_lts_t(lts_t *__source, event_t __tau,
                         semantic_model_t __semantic_model):
            _stage(PRENORMALIZING),
            _semantic_model(__semantic_model),
            _source(__source),
            _tau(__tau),
            _normalized(),
            states(),
            sets(),
            initial_map(),
            prenormalized()
        {
        }

        normalized_lts_t(const normalized_lts_t &other):
            _stage(other._stage),
            _semantic_model(other._semantic_model),
            _source(other._source),
            _tau(other._tau),
            _normalized(other._normalized),
            states(other.states),
            sets(other.sets),
            initial_map(other.initial_map),
            prenormalized(other.prenormalized)
        {
        }

        void clear(semantic_model_t __semantic_model)
        {
            _stage = PRENORMALIZING;
            _semantic_model = __semantic_model;
            _normalized.clear();
            states.clear();
            sets.clear();
            initial_map.clear();
            prenormalized.clear();
        }

        void clear()
        {
            clear(_semantic_model);
        }

        void swap(normalized_lts_t &other)
        {
            std::swap(_stage, other._stage);
            std::swap(_semantic_model, other._semantic_model);
            std::swap(_source, other._source);
            std::swap(_tau, other._tau);
            _normalized.swap(other._normalized);
            states.swap(other.states);
            sets.swap(other.sets);
            initial_map.swap(other.initial_map);
            prenormalized.swap(other.prenormalized);
        }

        void swap_sources(normalized_lts_t &other)
        {
            std::swap(_source, other._source);
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

        stage_t stage()
        {
            return _stage;
        }

        lts_t *source()
        {
            return _source;
        }

        const lts_t *source() const
        {
            return _source;
        }

        event_t tau() const
        {
            return _tau;
        }

        void tau(event_t __tau)
        {
            _tau = __tau;
        }

        semantic_model_t semantic_model() const
        {
            return _semantic_model;
        }

        state_t initial_normal_state(state_t initial_source)
        {
            initial_map_t::const_iterator  im_it =
                initial_map.find(initial_source);
            if (im_it == initial_map.end())
            {
                return HST_ERROR_STATE;
            } else {
                return im_it->second;
            }
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

        /**
         * Having prenormalized as many source states as necessary,
         * this method finalizes the normalization by finding any
         * normalized states that have the same behavior.  This is
         * done by bisimulating the normalized LTS; any states that
         * are identified as equivalent by this bisimulation are then
         * merge together in the normalized LTS.
         */

        void normalize();
    };

    std::ostream &operator << (std::ostream &stream,
                   const normalized_lts_t &normalized);
}

#endif // HST_NORMALIZED_LTS_H
