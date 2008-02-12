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

#ifndef HST_STATE_STATESET_MAP_HH
#define HST_STATE_STATESET_MAP_HH

#include <assert.h>
#include <tr1/memory>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/proxy-iterator.hh>
#include <hst/types.hh>

using namespace std;

namespace hst
{
    class state_stateset_map_t
    {
    protected:
        typedef judy_map_l<state_t, stateset_p,
                           state_t_hasher>       map_t;
        typedef shared_ptr<map_t>                map_p;

        map_t  map;

    public:
        /**
         * Dereference the map by an state, creating the corresponding
         * stateset if necessary.
         */

        stateset_p get(state_t state);

        /**
         * Dereference the map by an state, returning a NULL pointer
         * if the corresponding stateset doesn't exit.
         */

        stateset_cp get(state_t state) const;

        state_stateset_map_t():
            map()
        {
        }

        state_stateset_map_t(const state_stateset_map_t &other):
            map(other.map)
        {
        }

        void clear()
        {
            map.clear();
        }

        void swap(state_stateset_map_t &other)
        {
            map.swap(other.map);
        }

        state_stateset_map_t &operator =
        (const state_stateset_map_t &other)
        {
            if (this != &other)
            {
                clear();
                state_stateset_map_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        void add(state_t state1, state_t state2)
        {
            stateset_p  set = get(state1);
            *set += state2;
        }

        void erase(state_t state)
        {
            map.erase(state);
        }

        void erase(state_t state1, state_t state2)
        {
            stateset_p  set = get(state1);
            *set -= state2;

            if (set->size() == 0)
                erase(state1);
        }

    protected:
        /*
         * The judy_map_l iterator returns a (state_t, stateset_p)
         * pair, so we can get the state by taking the first element
         * of the pair.
         */

        struct states_evaluator
        {
            state_t operator () (map_t::const_iterator &it)
            {
                return it->first;
            }
        };

    public:
        typedef proxy_iterator<map_t::const_iterator,
                               state_t,
                               states_evaluator>
            states_iterator;

        states_iterator states_begin() const
        {
            map_t::const_iterator  it = map.begin();
            return states_iterator(it);
        }

        states_iterator states_end() const
        {
            return states_iterator();
        }

        class pairs_iterator
        {
        protected:
            map_t::const_iterator  se, se_end;
            stateset_t::iterator   et, et_end;

            state_state_t  current;

            void load_current()
            {
                if (se != se_end)
                    current.first = se->first;
                if (et != et_end)
                    current.second = *et;
            }

            void load_state_target()
            {
                if (se == se_end)
                    et = stateset_t::iterator();
                else
                    et = se->second->begin();
            }

            void advance()
            {
                // First find the next state target.
                ++et;

                // ...but if we've reached the last one, then we move
                // on to the next state.
                if (et == et_end)
                {
                    ++se;
                    load_state_target();
                }

                load_current();
            }

        public:
            pairs_iterator()
            {
            }

            pairs_iterator(map_t::const_iterator _se):
                se(_se)
            {
                load_state_target();
                load_current();
            }

            state_state_t operator * ()
            {
                return current;
            }

            state_state_t *operator -> ()
            {
                return &current;
            }

            pairs_iterator operator ++ (int)
            {
                pairs_iterator  ret = *this;
                this->operator ++ ();
                return ret;
            }

            pairs_iterator operator ++ ()
            {
                advance();
                return *this;
            }

            bool operator == (const pairs_iterator &other)
            {
                return
                    (this->se == other.se) &&
                    (this->et == other.et);
            }

            bool operator != (const pairs_iterator &other)
            {
                return
                    (this->se != other.se) ||
                    (this->et != other.et);
            }
        };

        pairs_iterator pairs_begin() const
        {
            map_t::const_iterator  it = map.begin();
            return pairs_iterator(it);
        }

        pairs_iterator pairs_end() const
        {
            return pairs_iterator();
        }

    };

    typedef shared_ptr<state_stateset_map_t>        state_stateset_map_p;
    typedef shared_ptr<const state_stateset_map_t>  state_stateset_map_cp;
}

#endif // HST_STATE_STATESET_MAP_H
