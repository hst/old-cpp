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

#ifndef HST_INTSETSET_HH
#define HST_INTSETSET_HH

#include <assert.h>
#include <iostream>

#include <boost/shared_ptr.hpp>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_l.h>

#include <hst/intset.hh>

#ifndef HST_INTSETSET_DEBUG
#define HST_INTSETSET_DEBUG 0
#endif

namespace hst
{
    typedef judy_set_l<intset_t, intset_t_hasher>  is_set;

    class intsetset_t
    {
    private:
        is_set         _set;
        unsigned long  _hash;

    public:
        /**
         * Union with another set
         */
        intsetset_t &operator |= (const intsetset_t &other);

        /**
         * Intersection with another set
         */
        intsetset_t &operator &= (const intsetset_t &other);

        /**
         * Difference of another set
         */
        intsetset_t &operator -= (const intsetset_t &other);

        /**
         * Containment of another set
         */
        bool operator >= (const intsetset_t &other) const;

        /**
         * Overlap with another set
         */
        bool overlaps_with(const intsetset_t &other) const;

        /**
         * Create a new, empty set
         */
        intsetset_t():
            _set(),
            _hash(0L)
        {
        }

        /**
         * Create a copy of an existing set
         */
        intsetset_t(const intsetset_t &other):
            _set(other._set),
            _hash(other._hash)
        {
        }

        /**
         * Remove all entries from a set
         */
        void clear()
        {
            _set.clear();
            _hash = 0L;
        }

        /**
         * Swap two sets
         */
        void swap(intsetset_t &other)
        {
            std::swap(_set, other._set);
            std::swap(_hash, other._hash);
        }

        /**
         * Assign another set into this one
         */
        intsetset_t &operator = (const intsetset_t &other)
        {
            if (this != &other)
            {
                clear();
                intsetset_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        /**
         * Destroy a set
         */
        ~intsetset_t()
        {
            clear();
        }

        /**
         * Return the hash value for this set.
         */
        unsigned long hash() const
        {
            return _hash;
        }

        /**
         * Add a single element
         */
        intsetset_t &operator += (const intset_t &element)
        {
            size_t  before, after;

            before = _set.size();
            _set.insert(element);
            after = _set.size();

            if (after > before)
            {
                // Only update the hash if the element wasn't already
                // in the set.

                _hash ^= element.hash();
            }

#if HST_INTSETSET_DEBUG
            cerr << "  Adding element " << element << ", hash now ";
            print_zobrist_key(cerr, _hash);
            cerr << endl;
#endif

            return *this;
        }

        /**
         * Remove a single element
         */
        intsetset_t &operator -= (const intset_t &element)
        {
            size_t         before, after;
            unsigned long  element_hash = element.hash();

#if HST_INTSETSET_DEBUG
            cerr << "  Removing element " << element;
#endif

            // If element comes from within the container, then it
            // will be invalidated by the call to erase.  So we cannot
            // use it as a value afterwards.

            before = _set.size();
            _set.erase(element);
            after = _set.size();

            if (after < before)
            {
                // Only update the hash if the element wasn't already
                // out of the set.

                _hash ^= element_hash;
            }

#if HST_INTSETSET_DEBUG
            cerr << ", hash now ";
            print_zobrist_key(cerr, _hash);
            cerr << endl;
#endif

            return *this;
        }

        /**
         * Check for containment of a single element
         */
        bool contains(const intset_t &element) const
        {
            return (_set.count(element) > 0);
        }

        /**
         * Find cardinality of set
         */
        unsigned long size() const
        {
            return _set.size();
        }

        /**
         * Set equality
         */
        bool operator == (const intsetset_t &other) const
        {
            // A set is trivially equal to itself.

            if (this == &other)
                return true;

            // If the sets' hashes mismatch, they can't be equal.

            if (this->_hash != other._hash)
                return false;

            // The hashes match, and the sets are distinct — they're
            // likely equal, but we still have to check the individual
            // elements to be certain.

            return (*this >= other) && (other >= *this);
        }

        typedef is_set::const_iterator  iterator;

        /**
         * Begin iterator
         */

        iterator begin() const
        {
            return _set.begin();
        }

        /**
         * End iterator
         */

        iterator end() const
        {
            return _set.end();
        }

    };

    typedef boost::shared_ptr<intsetset_t>        intsetset_p;
    typedef boost::shared_ptr<const intsetset_t>  intsetset_cp;

    struct intsetset_t_hasher
    {
        unsigned long operator () (const intsetset_t &intsetset) const
        {
            return intsetset.hash();
        }

        unsigned long operator () (intsetset_cp intsetset) const
        {
            return intsetset->hash();
        }

        bool operator () (intsetset_cp obj1, intsetset_cp obj2) const
        {
            if (obj1.get() == NULL)
                return obj2.get() == NULL;
            else
                return *obj1 == *obj2;
        }
    };

    // Input and output operators for streams
    std::istream &operator >> (std::istream &stream,
			       intsetset_t &intsetset);
    std::ostream &operator << (std::ostream &stream,
			       const intsetset_t &intsetset);
}

#endif // HST_INTSETSET_HH
