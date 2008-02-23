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

#ifndef EQUIVALENCE_CC
#define EQUIVALENCE_CC

#include <iostream>

#include <hst/equivalence.hh>

#ifndef EQUIV_DEBUG
#define EQUIV_DEBUG 0
#endif

using namespace std;

namespace hst
{
    void equivalences_t::add(state_t state, state_t head)
    {
        // The head of an equivalence class is always the one with
        // the lowest index.

        if (state < head)
        {
            std::swap(state, head);
        }

        head_map_t::iterator  it =
            _heads.find(state);

        if (it != _heads.end())
        {
            // This state was already in some equivalence class;
            // remove it from the old one before adding it to the
            // new.

#if EQUIV_DEBUG
            cerr << "old " << state
                 << " (old head = " << it->second
                 << ", new head = " << head
                 << ")" << endl;
#endif

            _members.erase(it->second, state);

            it->second = head;
            _members.add(head, state);
        } else {
#if EQUIV_DEBUG
            cerr << "new " << state << " (head " << head << ")" << endl;
#endif

            _heads.insert(make_pair(state, head));
            _members.add(head, state);
        }
    }

    void equivalences_t::merge(state_t new_head, state_t old_head)
    {
        // The head of an equivalence class is always the one with
        // the lowest index.

        if (old_head < new_head)
        {
            std::swap(new_head, old_head);
        }

        // Move each member of old_head's class into new_head's class.

        stateset_cp  old_members = members(old_head);
        stateset_t::iterator  it;

        for (it = old_members->begin();
             it != old_members->end();
             ++it)
        {
            add(*it, new_head);
        }
    }

    ostream &operator << (ostream &stream, const equivalences_t &equiv)
    {
        stream << "{";

        equivalences_t::heads_iterator  it;
        bool first = true;

        for (it = equiv.heads_begin();
             it != equiv.heads_end();
             ++it)
        {
            if (first)
            {
                stream << endl << "  ";
                first = false;
            } else {
                stream << "," << endl << "  ";
            }

            stateset_cp  members = equiv.members(*it);
            stream << *members;
        }

        stream << endl << "}" << endl;

        return stream;
    }
}

#endif // EQUIVALENCE_CC
