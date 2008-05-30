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

#ifndef INTSET_CC
#define INTSET_CC

#include <hst/intset.hh>

namespace hst
{
    zobrist_t intset_t::hash_keys;

    /**
     * Union with another set
     */
    intset_t &intset_t::operator |= (const intset_t &other)
    {
        // Loop through the elements in «other», adding each to
        // «this».

        for (intset_t::iterator it = other.begin();
             it != other.end(); ++it)
        {
            // Add this element to «this».
            *this += *it;
        }

        return *this;
    }

    /**
     * Intersection with another set
     */
    intset_t &intset_t::operator &= (const intset_t &other)
    {
        // Loop through the elements in this set.  Remove any that are
        // not also in «other».

        for (intset_t::iterator it = begin(); it != end(); ++it)
        {
            // See if this element is also set in «other».

            if (!other.contains(*it))
            {
                // It's not, so it should be removed from «this».
                *this -= *it;
            }
        }

        return *this;
    }

    /**
     * Difference with another set
     */
    intset_t &intset_t::operator -= (const intset_t &other)
    {
        // Loop through the elements in «other», removing each from
        // «this».

        for (intset_t::iterator it = other.begin();
             it != other.end(); ++it)
        {
            // Remove this element from «this».
            *this -= *it;
        }

        return *this;
    }

    /**
     * Overlap with another set
     */
    bool intset_t::overlaps_with(const intset_t &other) const
    {
        // Loop through the elements in «this», verifying that at
        // least one is also in «other».

        for (intset_t::iterator it = begin(); it != end(); ++it)
        {
            // Check whether this element is also in «other».

            if (other.contains(*it))
            {
                // It is, so the overlap succeeds.
                return true;
            }
        }

        // No element passed the test, so the overlap fails.
        return false;
    }

    ostream &operator << (ostream &stream, const intset_t &intset)
    {
        bool first = true;
        intset_t::iterator  end = intset.end();

        stream << '{';

        for (intset_t::iterator it = intset.begin();
             it != end; ++it)
        {
            if (first)
            {
                first = false;
            } else {
                stream << ',';
            }

            stream << *it;
        }

        stream << '}';

        return stream;
    }
}

#endif // INTSET_CC
