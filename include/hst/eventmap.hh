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

#ifndef HST_EVENTMAP_HH
#define HST_EVENTMAP_HH

#include <assert.h>
#include <iostream>

#include <boost/shared_ptr.hpp>

#include <judyarray/judy_funcs_wrappers.h>
#include <judy_set_cell.h>
#include <judy_map_l.h>

#include <hst/types.hh>

#ifndef HST_EVENTMAP_DEBUG
#define HST_EVENTMAP_DEBUG 0
#endif

namespace hst
{
    typedef std::pair<event_t, event_t>   eventpair_t;
    typedef boost::shared_ptr<eventpair_t>  eventpair_p;

    class eventmap_t
    {
    protected:
        typedef judy_map_l<event_t, alphabet_p,
                           event_t_hasher>       map_t;
        typedef boost::shared_ptr<map_t>                map_p;

        map_t  map;

        unsigned long  _size;

        /**
         * Dereference the event map, creating the inner alphabet if
         * necessary.
         */

        alphabet_p map_deref(event_t event);

        /**
         * Dereference the event map, return a NULL pointer if the
         * inner alphabet doesn't exist.
         */

        alphabet_p map_deref(event_t event) const;

    public:
        eventmap_t():
            _size(0)
        {
        }

        eventmap_t(const eventmap_t &other):
            map(other.map),
            _size(other._size)
        {
        }

        void clear()
        {
            map.clear();
            _size = 0L;
        }

        void swap(eventmap_t &other)
        {
            map.swap(other.map);
            std::swap(_size, other._size);
        }

        eventmap_t &operator = (const eventmap_t &other)
        {
            if (this != &other)
            {
                clear();
                eventmap_t  temp(other);
                swap(temp);
            }

            return *this;
        }

        unsigned long size()
        {
            return _size;
        }

        void insert(eventpair_t pair)
        {
            unsigned long  before, after;
            alphabet_p  alphabet = map_deref(pair.first);
            before = alphabet->size();

#if HST_EVENTMAP_DEBUG
            cerr << "old size " << _size << "/" << before << endl;
#endif

            *alphabet += pair.second;
            after = alphabet->size();
            _size = _size - before + after;

#if HST_EVENTMAP_DEBUG
            cerr << "new size " << _size << "/" << after << endl;
#endif
        }

        bool domain_contains(const event_t event) const
        {
            return (map.count(event) != 0);
        }

        typedef map_t::const_iterator  domain_iterator;

        domain_iterator domain_begin() const
        {
            return map.begin();
        }

        domain_iterator domain_end() const
        {
            return map.end();
        }

        typedef alphabet_t::iterator  image_iterator;

        image_iterator image_begin(event_t event) const
        {
            alphabet_p  alphabet = map_deref(event);
            if (alphabet.get() == NULL)
                return alphabet_t::iterator();
            else
                return alphabet->begin();
        }

        image_iterator image_end(event_t event) const
        {
            return alphabet_t::iterator();
        }

        class pair_iterator
        {
        protected:
            domain_iterator  domain, domain_end;
            image_iterator   image, image_end;
            eventpair_t      current;

            void load_current()
            {
                if (domain != domain_end)
                    current.first = domain->first;
                if (image != image_end)
                    current.second = *image;
            }

            void load_image()
            {
                if (domain == domain_end)
                    image = image_iterator();
                else
                    image = domain->second->begin();
            }

            void advance()
            {
                // First find the next element of the current image.
                ++image;

                // ...but if we've reached the end of the current
                // image, then we move on to the next element of the
                // domain.
                if (image == image_end)
                {
                    ++domain;
                    load_image();
                }

                load_current();
            }

        public:
            pair_iterator()
            {
            }

            pair_iterator(domain_iterator _domain):
                domain(_domain)
            {
                load_image();
                load_current();
            }

            eventpair_t operator * ()
            {
                return current;
            }

            eventpair_t *operator -> ()
            {
                return &current;
            }

            pair_iterator operator ++ (int)
            {
                pair_iterator  ret = *this;
                this->operator ++ ();
                return ret;
            }

            pair_iterator operator ++ ()
            {
                advance();
                return *this;
            }

            bool operator == (const pair_iterator &other)
            {
                return
                    (this->domain == other.domain) &&
                    (this->image == other.image);
            }

            bool operator != (const pair_iterator &other)
            {
                return
                    (this->domain != other.domain) ||
                    (this->image != other.image);
            }
        };

        pair_iterator pair_begin() const
        {
            return pair_iterator(map.begin());
        }

        pair_iterator pair_end() const
        {
            return pair_iterator();
        }
    };

    typedef boost::shared_ptr<eventmap_t>        eventmap_p;
    typedef boost::shared_ptr<const eventmap_t>  eventmap_cp;

    std::istream &operator >> (std::istream &stream,
                   eventmap_t &map);
    std::ostream &operator << (std::ostream &stream,
                   const eventmap_t &map);
}

#endif // HST_EVENTMAP_H
