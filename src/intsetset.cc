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

#ifndef INTSETSET_CC
#define INTSETSET_CC

#include <hst/intsetset.hh>
#include <hst/io.hh>
#include <hst/io-macros.hh>

namespace hst
{
    /**
     * Union with another set
     */
    intsetset_t& intsetset_t::operator |= (const intsetset_t& other)
    {
        // Loop through the elements in «other», adding each to
        // «this».

        for (intsetset_t::iterator it = other.begin();
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
    intsetset_t& intsetset_t::operator &= (const intsetset_t& other)
    {
        // Loop through the elements in this set.  Remove any that are
        // not also in «other».

        for (intsetset_t::iterator it = begin(); it != end(); ++it)
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
    intsetset_t& intsetset_t::operator -= (const intsetset_t& other)
    {
        // Loop through the elements in «other», removing each from
        // «this».

        for (intsetset_t::iterator it = other.begin();
             it != other.end(); ++it)
        {
            // Remove this element from «this».
            *this -= *it;
        }

        return *this;
    }

    /**
     * Containment of another set
     */
    bool intsetset_t::operator >= (const intsetset_t& other) const
    {
        // Loop through the elements in «other», verifying that each
        // is also in «this».

        for (intsetset_t::iterator it = other.begin();
             it != other.end(); ++it)
        {
            // Check whether this element is also in «this».

            if (!this->contains(*it))
            {
                // It's not, so the containment fails.
                return false;
            }
        }

        // No element failed the test, so the containment succeeds.
        return true;
    }

    /**
     * Overlap with another set
     */
    bool intsetset_t::overlaps_with(const intsetset_t& other) const
    {
        // Loop through the elements in «this», verifying that at
        // least one is also in «other».

        for (intsetset_t::iterator it = begin(); it != end(); ++it)
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

    istream& operator >> (istream& stream, intsetset_t& intsetset)
    {
        intsetset_t  result;
        intset_t     element;

        // Clear the intsetset in case it has stale values from
        // before.

        result.clear();

        // First try to read the opening curly brace.

        require_char(stream, '{', true);
        PROPAGATE_ANY_ERROR(stream);

        // For the first element, we can either match a closing curly
        // brace, signifying the empty set, or we can match an
        // integer.

        require_char(stream, '}', true);
        EOF_IS_ERROR;
        PROPAGATE_IO_ERROR(stream);

        if (!stream.fail())
        {
            intsetset.swap(result);
            return stream;
        }

        // It wasn't a brace, so it should be an intset.

        stream.clear();
        stream >> element;
        EOF_IS_ERROR;
        PROPAGATE_ANY_ERROR(stream);

        result += element;

        // For the remaining elements, we must either match a closing
        // curly brace, or a comma followed by an intset.  Anything
        // else is a parse error.

        while (true)
        {
            require_char(stream, '}', true);
            EOF_IS_ERROR;
            PROPAGATE_IO_ERROR(stream);

            if (!stream.fail())
            {
                intsetset.swap(result);
                return stream;
            }

            // It wasn't a brace, so it should be a comma followed by
            // an intset.

            stream.clear();
            require_char(stream, ',', true);
            EOF_IS_ERROR;
            PROPAGATE_ANY_ERROR(stream);

            stream >> element;
            EOF_IS_ERROR;
            PROPAGATE_ANY_ERROR(stream);

            result += element;
        }
    }

    ostream& operator << (ostream& stream, const intsetset_t& intsetset)
    {
        bool first = true;
        intsetset_t::iterator  end = intsetset.end();

        stream << '{';

        for (intsetset_t::iterator it = intsetset.begin();
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

#endif // INTSETSET_CC
