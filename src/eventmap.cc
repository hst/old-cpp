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

#ifndef EVENTMAP_CC
#define EVENTMAP_CC

#include <hst/eventmap.hh>

using namespace std;

namespace hst
{
    alphabet_p eventmap_t::map_deref(event_t event)
    {
        alphabet_p  alphabet;
        pair<map_t::iterator, bool>  insert_result;

#if HST_EVENTMAP_DEBUG
        cerr << "Deref ("
             << event << ")"
             << endl;
#endif

        // Try to insert a NULL alphabet into the map.  This will tell
        // us whether there's already an alphabet for this event.

        insert_result =
            map.insert(make_pair(event, alphabet));

        if (insert_result.second)
        {
            // The insert succeeded, so there's currently a NULL
            // pointer in the map for this event.  We need to change
            // this NULL to a pointer to an actual alphabet; luckily,
            // we've got an iterator we can use to do this, though
            // it's a bit tricky.

#if HST_EVENTMAP_DEBUG
            cerr << "  new = ";
#endif
            insert_result.first->second.reset(new alphabet_t);
        } else {
            // The insert failed because there's already an alphabet.
            // Moreover, the iterator points to the alphabet's
            // smart_ptr, so there's not really anything to do here.

#if HST_EVENTMAP_DEBUG
            cerr << "  old = ";
#endif
        }

#if HST_EVENTMAP_DEBUG
        cerr << insert_result.first->second.get() << endl;
#endif
        return insert_result.first->second;
    }

    alphabet_p eventmap_t::map_deref(event_t event) const
    {
#if HST_EVENTMAP_DEBUG
        cerr << "Deref const ("
             << event << ")"
             << endl;
#endif

        map_t::const_iterator  it = map.find(event);

        if (it == map.end())
        {
#if HST_EVENTMAP_DEBUG
            cerr << "  new" << endl;
#endif
            return alphabet_p();
        } else {
#if HST_EVENTMAP_DEBUG
            cerr << "  old = " << it->second << endl;
#endif
            return alphabet_p(it->second);
        }
    }

    ostream &operator << (ostream &stream, const eventmap_t &map)
    {
        bool first = true;
        eventmap_t::pair_iterator  end = map.pair_end();

        stream << '{';

        for (eventmap_t::pair_iterator it = map.pair_begin();
             it != end; ++it)
        {
            if (first)
            {
                first = false;
            } else {
                stream << ',';
            }

            stream << it->first << "->" << it->second;
        }

        stream << '}';

        return stream;
    }
}

#endif // EVENTMAP_CC
