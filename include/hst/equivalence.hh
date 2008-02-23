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

#ifndef HST_EQUIVALENCE_HH
#define HST_EQUIVALENCE_HH

#include <assert.h>
#include <iostream>
#include <tr1/memory>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/types.hh>
#include <hst/proxy-iterator.hh>
#include <hst/state-stateset-map.hh>

namespace hst
{
    class equivalences_t
    {
    protected:
        typedef judy_map_l<state_t, state_t,
                           state_t_hasher>    head_map_t;
        typedef shared_ptr<head_map_t>        head_map_p;

        /**
         * Records the “head” of the equivalence class that [state]
         * belongs to.  The head of the class is the state with the
         * lowest index.
         */

        head_map_t  _heads;

        /**
         * Records the members of each equivalence class.  The class
         * is keyed by its head member.
         */

        state_stateset_map_t  _members;

    public:
        equivalences_t()
        {
        }

        equivalences_t(const equivalences_t &other):
            _heads(other._heads),
            _members(other._members)
        {
        }

        void clear()
        {
            _heads.clear();
            _members.clear();
        }

        void swap(equivalences_t &other)
        {
            _heads.swap(other._heads);
            _members.swap(other._members);
        }

        equivalences_t &operator = (const equivalences_t &other)
        {
            if (this != &other)
            {
                clear();
                equivalences_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        state_t head(state_t state) const
        {
            head_map_t::const_iterator  it =
                _heads.find(state);

            if (it == _heads.end())
            {
                return HST_ERROR_STATE;
            } else {
                return it->second;
            }
        }

        stateset_cp members(state_t head) const
        {
            // Use this const reference to force us to use the const
            // version of get()

            const state_stateset_map_t  &const_members = _members;
            return const_members.get(head);
        }

        bool equivalent(state_t state1, state_t state2) const
        {
            return
                (head(state1) != HST_ERROR_STATE) &&
                (head(state1) == head(state2));
        }

        void add(state_t state, state_t head);
        void merge(state_t new_head, state_t old_head);

        typedef state_stateset_map_t::states_iterator
            heads_iterator;

        heads_iterator heads_begin() const
        {
            return _members.states_begin();
        }

        heads_iterator heads_end() const
        {
            return _members.states_end();
        }

    protected:
        /*
         * The judy_map_l iterator returns a (state_t, state_t) pair,
         * so we can get the right state by taking the first element
         * of the pair.
         */

        struct range_evaluator
        {
            state_t operator () (head_map_t::const_iterator &it)
            {
                return it->first;
            }
        };

    public:
        typedef proxy_iterator<head_map_t::const_iterator, state_t,
                               range_evaluator>
            range_iterator;

        range_iterator range_begin() const
        {
            head_map_t::const_iterator  it = _heads.begin();
            return range_iterator(it);
        }

        range_iterator range_end() const
        {
            return range_iterator();
        }

        typedef head_map_t::const_iterator  pairs_iterator;

        pairs_iterator pairs_begin() const
        {
            return _heads.begin();
        }

        pairs_iterator pairs_end() const
        {
            return _heads.end();
        }
    };

    ostream &operator << (ostream &stream, const equivalences_t &equiv);
}

#endif // HST_EQUIVALENCE_H
