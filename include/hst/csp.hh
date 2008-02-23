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
#include <sstream>
#include <string>
#include <tr1/memory>

#include <hst/types.hh>
#include <hst/eventmap.hh>
#include <hst/lts.hh>
#include <hst/normalized-lts.hh>

#ifndef HST_CSP_DEBUG
#define HST_CSP_DEBUG 0
#endif

using namespace std;

namespace hst
{
    enum csp_operator_t
    {
        PREFIX = 1,
        EXTCHOICE,
        INTCHOICE,
        INTERRUPT,
        SEQCOMP,
        INTERLEAVE,
        INTERFACE_PARALLEL,
        ALPHABETIZED_PARALLEL,
        HIDE,
        RENAME
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

        unsigned long  _next_temp_index;

        state_name_map_t  _memoized_processes;

        normalized_lts_t  _normalized_lts;

        void define_standard_ops()
        {
            /*
             * Create the predefined CSP states and events.
             */

            _stop = add_process("STOP");
            _skip = add_process("SKIP");
            _tau  = add_event("%TAU");
            _tick = add_event("%TICK");

            _lts.add_edge(_skip, _tick, _stop);

            _lts.finalize(_stop);
            _lts.finalize(_skip);
        }

    public:
        csp_t():
            _lts(),
            _next_temp_index(0L),
            _normalized_lts(&_lts, 0L)
        {
            define_standard_ops();
            _normalized_lts.tau(_tau);
        }

        csp_t(const csp_t &other):
            _lts(other._lts),
            _stop(other._stop),
            _skip(other._skip),
            _tau(other._tau),
            _tick(other._tick),
            _state_symbol_table(other._state_symbol_table),
            _event_symbol_table(other._event_symbol_table),
            _next_temp_index(0L),
            _memoized_processes(other._memoized_processes),
            _normalized_lts(other._normalized_lts)
        {
        }

        void clear()
        {
            _lts.clear();
            _state_symbol_table.clear();
            _event_symbol_table.clear();
            _next_temp_index = 0L;
            _memoized_processes.clear();
            _normalized_lts.clear();
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
            std::swap(_next_temp_index, other._next_temp_index);
            _memoized_processes.swap(other._memoized_processes);
            _normalized_lts.swap(other._normalized_lts);

            // We've just swapped _normalized_lts._source twice; the
            // ._source pointers were swapped, while the contents of
            // those pointers were also swapped.  Let's swap the
            // source pointers back to fix this.

            _normalized_lts.swap_sources(other._normalized_lts);
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

        lts_t *lts()
        {
            return &_lts;
        }

        const lts_t *lts() const
        {
            return &_lts;
        }

        normalized_lts_t *normalized_lts()
        {
            return &_normalized_lts;
        }

        const normalized_lts_t *normalized_lts() const
        {
            return &_normalized_lts;
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

        state_t add_temp_process()
        {
            ostringstream  name;
            name << "%" << _next_temp_index++;
            return add_process(name.str());
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

        void alias_process(const string &name, state_t process)
        {
#if HST_CSP_DEBUG
            cerr << "Aliasing process " << name
                 << " = " << process << endl;
#endif

            _state_symbol_table.insert(make_pair(name, process));
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

        state_t lookup_memoized_process(const string &name) const
        {
            state_name_map_t::const_iterator  it =
                _memoized_processes.find(name);

            if (it == _memoized_processes.end())
            {
                return HST_ERROR_STATE;
            } else {
                return it->second;
            }
        }

        void save_memoized_process(const string &name, state_t state)
        {
            _memoized_processes.insert(make_pair(name, state));
        }

        /// [a→P]
        state_t add_prefix(event_t a, state_t P);
        void prefix(state_t dest, event_t a, state_t P);

        /// [P□Q]
        state_t add_extchoice(state_t P, state_t Q);
        void extchoice(state_t dest, state_t P, state_t Q);

        /// [P⊓Q]
        state_t add_intchoice(state_t P, state_t Q);
        void intchoice(state_t dest, state_t P, state_t Q);

        /// [P▵Q]
        state_t add_interrupt(state_t P, state_t Q);
        void interrupt(state_t dest, state_t P, state_t Q);

        /// [P;Q]
        state_t add_seqcomp(state_t P, state_t Q);
        void seqcomp(state_t dest, state_t P, state_t Q);

        /// [P ||| Q]
        state_t add_interleave(state_t P, state_t Q);
        void interleave(state_t dest, state_t P, state_t Q);

        /// [P〚A〛Q]
        state_t add_interface_parallel
        (state_t P, alphabet_t &alpha, state_t Q);
        void interface_parallel
        (state_t dest, state_t P, alphabet_t &alpha, state_t Q);

        /// [P [αP‖αQ] Q]
        state_t add_alphabetized_parallel
        (state_t P, alphabet_t &alphaP, alphabet_t &alphaQ, state_t Q);
        void alphabetized_parallel
        (state_t dest,
         state_t P, alphabet_t &alphaP, alphabet_t &alphaQ, state_t Q);

        /// [P \ α]
        state_t add_hide
        (state_t P, alphabet_t &alpha);
        void hide
        (state_t dest,
         state_t P, alphabet_t &alpha);

        /// [P \ μ]
        state_t add_rename
        (state_t P, eventmap_t &map);
        void rename
        (state_t dest,
         state_t P, eventmap_t &map);
    };

    typedef shared_ptr<csp_t>        csp_p;
    typedef shared_ptr<const csp_t>  csp_cp;

    void read_csp0(istream &stream, csp_t &csp);

    ostream &operator << (ostream &stream, const csp_t &csp);
}

#endif // HST_CSP_HH
