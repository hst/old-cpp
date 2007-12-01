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

#include <iostream>
#include <iterator>
#include <tr1/memory>

#include <Judy.h>

#include <hst/zobrist.hh>

#ifndef HST_INTSET_DEBUG
#define HST_INTSET_DEBUG 0
#endif

using namespace std;
using namespace std::tr1;

namespace hst
{
    class intset_t
    {
    private:
        Pvoid_t  set;

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
        intset_t()
        {
            set = NULL;
            _hash = 0L;
        }

        /**
         * Create a copy of an existing set
         */
        intset_t(const intset_t &other)
        {
            set = NULL;
            _hash = 0L;
            *this |= other;
        }

        /**
         * Remove all entries from a set
         */
        void clear()
        {
            Word_t  size;
            J1FA(size, set);
            _hash = 0L;
        }

        /**
         * Swap two sets
         */
        void swap(intset_t &other)
        {
            std::swap(set, other.set);
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
            int  rc_int;
            J1S(rc_int, set, element);

            if (rc_int == 1)
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

            // TODO: If there was a malloc error adding this element,
            // we should throw an exception.

            return *this;
        }

        /**
         * Remove a single element
         */
        intset_t &operator -= (const unsigned long element)
        {
            int  rc_int;
            J1U(rc_int, set, element);

            if (rc_int == 1)
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

            // TODO: If there was a malloc error removing this
            // element, we should throw an exception.

            return *this;
        }

        /**
         * Check for containment of a single element
         */
        bool contains(const unsigned long element) const
        {
            int  rc_int;
            J1T(rc_int, set, element);
            return rc_int;
        }

        /**
         * Find cardinality of set
         */
        unsigned long cardinality() const
        {
            Word_t  result;
            J1C(result, set, 0, (Word_t) -1);
            return result;
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

        /**
         * Iterator
         */

        class iterator:
            public std::iterator<forward_iterator_tag,
                                 unsigned long>
        {
        private:
            const intset_t  &intset;
            unsigned long   last_read;
            bool            finished;

            void print()
            {
                if (finished)
                    cerr << ((void*) this) << " Iterator finished" << endl;
                else
                    cerr << ((void*) this) <<
                        " Iterator now at " << last_read << endl;
            }

            void advance_first()
            {
                int  rc_int;
                J1F(rc_int, intset.set, last_read);
                finished = (rc_int == 0);
            }

            void advance()
            {
                int  rc_int;
                J1N(rc_int, intset.set, last_read);
                finished = (rc_int == 0);
            }

        public:
            iterator(const intset_t &_intset):
                intset(_intset)
            {
                last_read = 0;
                advance_first();
            }

            iterator(const intset_t &_intset, unsigned long initial):
                intset(_intset)
            {
                last_read = initial;
                advance_first();
            }

            iterator(const intset_t &_intset, bool _finished):
                intset(_intset)
            {
                last_read = 0;
                finished = _finished;
                if (!finished)
                {
                    advance_first();
                }
            }

            ~iterator()
            {
            }

            iterator &operator = (const iterator &other)
            {
                last_read = other.last_read;
                finished = other.finished;
            }

            bool operator == (const iterator &other) const
            {
                // If both are finished, then they're equal.
                if (finished && other.finished) return true;

                // If one is finished and the other not, they're
                // unequal.
                if (finished != other.finished) return false;

                // If neither is finished, then they must be at the
                // same location.
                return (last_read == other.last_read);
            }

            bool operator != (const iterator &other) const
            {
                return !(*this == other);
            }

            iterator &operator ++ ()
            {
                advance();
            }

            iterator &operator ++ (int)
            {
                advance();
            }

            unsigned long operator * ()
            {
                return last_read;
            }

        };

        /**
         * Begin iterator
         */

        iterator begin() const
        {
            return iterator(*this);
        }

        /**
         * End iterator
         */

        iterator end() const
        {
            return iterator(*this, true);
        }

    };

    typedef shared_ptr<intset_t>  intset_p;

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
