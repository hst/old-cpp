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

#ifndef HST_CSP_HH
#define HST_CSP_HH

#include <iostream>
#include <string>
#include <tr1/memory>

#include <hst/types.hh>
#include <hst/lts.hh>

#ifndef HST_CSP_DEBUG
#define HST_CSP_DEBUG 0
#endif

using namespace std;

namespace hst
{
    enum csp_operator_t
    {
        PREFIX = 1
    };

    class csp_t
    {
    protected:
        lts_t  _lts;

        state_t  _stop;
        state_t  _skip;

        event_t  _tau;
        event_t  _tick;

        typedef judy_map_l<string, state_t,
                           string_hasher>     state_name_map_t;
        typedef shared_ptr<state_name_map_t>  state_name_map_p;

        state_name_map_t  _state_symbol_table;

        typedef judy_map_l<string, event_t,
                           string_hasher>     event_name_map_t;
        typedef shared_ptr<event_name_map_t>  event_name_map_p;

        event_name_map_t  _event_symbol_table;

        void define_standard_ops()
        {
            /*
             * Create the predefined CSP states and events.
             */

            _stop = add_process("STOP");
            _skip = add_process("SKIP");
            _tau  = add_event("TAU");
            _tick = add_event("TICK");

            _lts.finalize(_stop);
            _lts.finalize(_skip);
        }

    public:
        csp_t()
        {
            define_standard_ops();
        }

        csp_t(const csp_t &other):
            _lts(other._lts),
            _stop(other._stop),
            _skip(other._skip),
            _tau(other._tau),
            _tick(other._tick),
            _state_symbol_table(other._state_symbol_table),
            _event_symbol_table(other._event_symbol_table)
        {
        }

        void clear()
        {
            _lts.clear();
            _state_symbol_table.clear();
            _event_symbol_table.clear();
            define_standard_ops();
        }

        void swap(csp_t &other)
        {
            _lts.swap(other._lts);
            std::swap(_stop, other._stop);
            std::swap(_skip, other._skip);
            std::swap(_tau, other._tau);
            std::swap(_tick, other._tick);
            _state_symbol_table.swap(other._state_symbol_table);
            _event_symbol_table.swap(other._event_symbol_table);
        }

        csp_t &operator = (const csp_t &other)
        {
            if (this != &other)
            {
                clear();
                csp_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        lts_t lts() const
        {
            return _lts;
        }

        state_t stop() const
        {
            return _stop;
        }

        state_t skip() const
        {
            return _skip;
        }

        event_t tau() const
        {
            return _tau;
        }

        event_t tick() const
        {
            return _tick;
        }

        state_t add_process(const string &name)
        {
#if HST_CSP_DEBUG
            cerr << "Adding process " << name;
#endif

            state_t state = _lts.add_state(name);

#if HST_CSP_DEBUG
            cerr << " = " << state << endl;
#endif

            _state_symbol_table.insert(make_pair(name, state));
            return state;
        }

        event_t add_event(const string &name)
        {
#if HST_CSP_DEBUG
            cerr << "Adding event " << name;
#endif

            event_t event = _lts.add_event(name);

#if HST_CSP_DEBUG
            cerr << " = " << event << endl;
#endif

            _event_symbol_table.insert(make_pair(name, event));
            return event;
        }

        state_t get_process(const string &name) const
        {
#if HST_CSP_DEBUG
            cerr << "Retrieving process " << name;
#endif

            state_name_map_t::const_iterator  it =
                _state_symbol_table.find(name);

            if (it == _state_symbol_table.end())
            {
#if HST_CSP_DEBUG
                cerr << ", not found" << endl;
#endif
                return HST_ERROR_STATE;
            } else {
#if HST_CSP_DEBUG
                cerr << " = " << it->second << endl;
#endif
                return it->second;
            }
        }

        event_t get_event(const string &name) const
        {
#if HST_CSP_DEBUG
            cerr << "Retrieving event " << name;
#endif

            event_name_map_t::const_iterator  it =
                _event_symbol_table.find(name);

            if (it == _event_symbol_table.end())
            {
#if HST_CSP_DEBUG
                cerr << ", not found" << endl;
#endif
                return HST_ERROR_EVENT;
            } else {
#if HST_CSP_DEBUG
                cerr << " = " << it->second << endl;
#endif
                return it->second;
            }
        }

        /// [a→P]
        void prefix(state_t dest,
                    event_t a, state_t P);
    };

    typedef shared_ptr<csp_t>  csp_p;

    void read_csp0(istream &stream, csp_t &csp);

    ostream &operator << (ostream &stream, const csp_t &csp);
}

#endif // HST_CSP_HH
