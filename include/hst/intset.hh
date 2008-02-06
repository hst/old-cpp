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

#ifndef HST_INTSET_HH
#define HST_INTSET_HH

#include <assert.h>
#include <iostream>
#include <tr1/memory>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>

#include <hst/zobrist.hh>

#ifndef HST_INTSET_DEBUG
#define HST_INTSET_DEBUG 0
#endif

using namespace std;
using namespace std::tr1;

namespace hst
{
    typedef judy_set_cell<const unsigned long>  ulong_set;

    class intset_t
    {
    private:
        ulong_set      _set;
        unsigned long  _hash;

        static zobrist_t  hash_keys;

    public:
        /**
         * Union with another set
         */
        intset_t &operator |= (const intset_t &other);

        /**
         * Intersection with another set
         */
        intset_t &operator &= (const intset_t &other);

        /**
         * Difference of another set
         */
        intset_t &operator -= (const intset_t &other);

        /**
         * Containment of another set
         */
        bool operator >= (const intset_t &other) const;

        /**
         * Overlap with another set
         */
        bool overlaps_with(const intset_t &other) const;

        /**
         * Create a new, empty set
         */
        intset_t():
            _set(),
            _hash(0L)
        {
        }

        /**
         * Create a copy of an existing set
         */
        intset_t(const intset_t &other):
            _set(other._set),
            _hash(other._hash)
        {
        }

        /**
         * Create a new set from a couple of iterators
         */
        template <class Iterator>
        intset_t(Iterator begin, Iterator end):
            _set(),
            _hash(0L)
        {
            Iterator  it = begin;
            for (; it != end; ++it)
                *this += *it;
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
        void swap(intset_t &other)
        {
            std::swap(_set, other._set);
            std::swap(_hash, other._hash);
        }

        /**
         * Assign another set into this one
         */
        intset_t &operator = (const intset_t &other)
        {
            if (this != &other)
            {
                clear();
                intset_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        /**
         * Destroy a set
         */
        ~intset_t()
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
        intset_t &operator += (const unsigned long element)
        {
            size_t  before, after;

            before = _set.size();
            _set.insert(element);
            after = _set.size();

            if (after > before)
            {
                // Only update the hash if the element wasn't already
                // in the set.

                _hash ^= hash_keys.get_key(element);
            }

#if HST_INTSET_DEBUG
            cerr << "  Adding element " << element << ", hash now ";
            print_zobrist_key(cerr, _hash);
            cerr << endl;
#endif

            return *this;
        }

        /**
         * Remove a single element
         */
        intset_t &operator -= (const unsigned long element)
        {
            size_t  before, after;

            before = _set.size();
            _set.erase(element);
            after = _set.size();

            if (after < before)
            {
                // Only update the hash if the element wasn't already
                // out of the set.

                _hash ^= hash_keys.get_key(element);
            }

#if HST_INTSET_DEBUG
            cerr << "  Removing element " << element << ", hash now ";
            print_zobrist_key(cerr, _hash);
            cerr << endl;
#endif

            return *this;
        }

        /**
         * Check for containment of a single element
         */
        bool contains(const unsigned long element) const
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
        bool operator == (const intset_t &other) const
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

        typedef ulong_set::iterator  iterator;

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

    typedef shared_ptr<intset_t>        intset_p;
    typedef shared_ptr<const intset_t>  intset_cp;

    struct intset_t_hasher
    {
        unsigned long operator () (const intset_t &intset) const
        {
            return intset.hash();
        }
    };

    // Input and output operators for streams
    istream &operator >> (istream &stream, intset_t &intset);
    ostream &operator << (ostream &stream, const intset_t &intset);
}

#endif // HST_INTSET_HH
