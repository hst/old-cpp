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

#ifndef HST_PROXY_ITERATOR_HH
#define HST_PROXY_ITERATOR_HH

namespace hst
{
    /*
     * Many of our iterators have basically the same format, so we'll
     * define it once using a template.  They all use an underlying
     * iterator to perform all of the work, possibly modifying the
     * result before it's returned.  «Iterator» is the base type of
     * the underlying iterator.  «Result» is the value return by this
     * iterator.  «Eval» is a functor that returns the current value
     * from the underlying iterator.
     */

    template<typename Iterator,
             typename Result,
             typename Eval>
    class proxy_iterator
    {
    private:
        typedef proxy_iterator<Iterator, Result, Eval>  this_type;

        Eval      eval;
        Iterator  it;

    public:
        proxy_iterator()
        {
        }

        proxy_iterator(Iterator &_it):
            it(_it)
        {
        }

        Result operator * ()
        {
            return eval(it);
        }

        this_type operator ++ (int)
        {
            this_type  ret = *this;
            ++it;
            return ret;
        }

        this_type &operator ++ ()
        {
            ++it;
            return *this;
        }

        bool operator == (const this_type &other)
        {
            return (this->it == other.it);
        }

        bool operator != (const this_type &other)
        {
            return (this->it != other.it);
        }
    };
}

#endif // HST_PROXY_ITERATOR_H
