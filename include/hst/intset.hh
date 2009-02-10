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

#ifndef HST_INTSET_HH
#define HST_INTSET_HH

#include <assert.h>
#include <iostream>

#include <boost/shared_ptr.hpp>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>

#include <hst/zobrist.hh>

#ifndef HST_INTSET_DEBUG
#define HST_INTSET_DEBUG 0
#endif

namespace hst
{
    // Containment of sets
    //
    // Returns true if intset(begin1, end1) would be a superset of
    // intset(begin2, end2).  Each iterator is expected to return the
    // contents of its set in sorted order.  The result is a pair.
    // The car of the pair is (set1 >= set2).  If this is false, the
    // cdr of the pair is some element of set1 that is not in set2.

    template <class I1, class I2>
    std::pair<bool, unsigned long>
    is_superset_with_proof(const I1 &begin1, const I1 &end1,
                           const I2 &begin2, const I2 &end2)
    {
        I1  it1 = begin1;
        I2  it2 = begin2;

        // Verify that there is no element in set2 that is not also in
        // set1.

        while (it1 != end1 && it2 != end2)
        {
            if (*it1 > *it2)
            {
                // We've advanced it1 further than it2 without seeing
                // *it2 in set1.  This means that *it2 cannot be in
                // set1, violating the superset constraint.

                return std::make_pair(false, *it2);
            } else if (*it1 < *it2) {
                // We've advanced it2 further than it1 without seeing
                // *it1 in set2.  This means the *it1 cannot be in
                // set2.  But this is okay, so advance it1 and
                // continue the check.

                ++it1;
            } else {
                // We've found *it2 in set1, which is good.  Advance
                // it2 to get the next element to check.  While we're
                // at it, we advance it1, as well; since we're
                // checking sets, we know that *it1 won't appear
                // again, so we don't need it anymore.

                ++it1;
                ++it2;
            }
        }

        // We made it through one of the iterators.  If it2 has
        // finished, then we're golden, since we verified that all of
        // set2's elements were in set1.  If it's not, then we reached
        // the end of set1 even though we still had set2 elements to
        // check, which means the superset fails.

        if (it2 == end2)
        {
            return std::make_pair(true, 0);
        } else {
            return std::make_pair(false, *it2);
        }
    }

    // A version of is_superset that only returns the boolean result.

    template <class I1, class I2>
    bool is_superset(const I1 &begin1, const I1 &end1,
                     const I2 &begin2, const I2 &end2)
    {
        return is_superset_with_proof(begin1, end1, begin2, end2).first;
    }

    // Equality of sets
    //
    // Returns true if intset(begin1, end1) would be equal to
    // intset(begin2, end2).  Each iterator is expected to return the
    // contents of its set in sorted order.

    template <class I1, class I2>
    bool sets_equal(const I1 &begin1, const I1 &end1,
                    const I2 &begin2, const I2 &end2)
    {
        I1  it1 = begin1;
        I2  it2 = begin2;

        // Verify that there is no element in set2 that is not also in
        // set1, and vice versa.

        while (it1 != end1 && it2 != end2)
        {
            if (*it1 != *it2)
            {
                // We've advanced it1 further than it2 without seeing
                // *it2 in set1, or vice versa.  This means that the
                // current value of the iterator that's further along
                // can't be in the other set; therefore, the sets are
                // not equal.

                return false;
            } else {
                // The current value is in both sets, which is good.
                // Advance the two iterators to the next value to
                // check.

                ++it1;
                ++it2;
            }
        }

        // We made it through one at least one of the iterators.  The
        // sets are equal if they're *both* at the end; if not, then
        // one of the two sets has an extra element.

        return (it1 == end1 && it2 == end2);
    }

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
         * Containment of another set
         */
        bool operator >= (const intset_t &other) const
        {
            return is_superset(begin(), end(),
                               other.begin(), other.end());
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

            return sets_equal(begin(), end(), other.begin(), other.end());
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

    typedef boost::shared_ptr<intset_t>        intset_p;
    typedef boost::shared_ptr<const intset_t>  intset_cp;

    struct intset_t_hasher
    {
        unsigned long operator () (const intset_t &intset) const
        {
            return intset.hash();
        }

        unsigned long operator () (intset_cp intset) const
        {
            return intset->hash();
        }

        bool operator () (intset_cp obj1, intset_cp obj2) const
        {
            if (obj1.get() == NULL)
                return obj2.get() == NULL;
            else
                return *obj1 == *obj2;
        }
    };

    // Input and output operators for streams
    std::istream &operator >> (std::istream &stream,
                   intset_t &intset);
    std::ostream &operator << (std::ostream &stream,
                   const intset_t &intset);
}

#endif // HST_INTSET_HH
