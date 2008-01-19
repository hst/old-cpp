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

#ifndef HST_CSP_MACROS_HH
#define HST_CSP_MACROS_HH

#if HST_CSP_DEBUG

#define REQUIRE_FINALIZED(P)                        \
    if (!_lts.is_finalized(P))                      \
    {                                               \
        cerr << "  *** " << (P)                     \
             << " NOT FINALIZED!" << endl;          \
                                                    \
        return;                                     \
    }

#define REQUIRE_NOT_FINALIZED(P)                    \
    if (_lts.is_finalized(P))                       \
    {                                               \
        cerr << "  *** " << (P)                     \
             << " ALREADY FINALIZED!" << endl;      \
                                                    \
        return;                                     \
    }

#else

#define REQUIRE_FINALIZED(P)                        \
    if (!_lts.is_finalized(P))                      \
    {                                               \
        return;                                     \
    }

#define REQUIRE_NOT_FINALIZED(P)                    \
    if (_lts.is_finalized(P))                       \
    {                                               \
        return;                                     \
    }

#endif

#endif // HST_CSP_MACROS_HH
