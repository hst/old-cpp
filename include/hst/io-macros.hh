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

#ifndef HST_IO_MACROS_HH
#define HST_IO_MACROS_HH

/***********************************************************************
 * Some helper macros for the parser functions.
 *
 * NOTE: Some of the macros take in a value that's used in the return
 * statement.  If they're used in the middle of a void function, you
 * can use "(void) 0" as the return value to make everything compile
 * correctly.
 */

/*
 * Signal parse errors by setting the stream's fail bit.
 */

#define PARSE_ERROR(ret)                        \
    {                                           \
        stream.setstate(ios_base::failbit);     \
        return (ret);                           \
    }

/*
 * Used immediately before one of the PROPAGATE_ or FINALIZE_ macros
 * to signal that an EOF should be treated as a parse error.
 */

#define EOF_IS_ERROR                            \
    {                                           \
        if (stream.eof())                       \
        {                                       \
            stream.setstate(ios_base::failbit); \
        }                                       \
    }

/*
 * Immediately aborts the parsing function if any non-success code is
 * found.
 */
    
#define PROPAGATE_ANY_ERROR(ret)                \
    {                                           \
        if (!stream.good())                     \
        {                                       \
            return (ret);                       \
        }                                       \
    }

/*
 * Immediately aborts the parsing function if any I/O exception is
 * found.
 */

#define PROPAGATE_IO_ERROR(ret)                 \
    {                                           \
        if (stream.eof() || stream.bad())       \
        {                                       \
            return (ret);                       \
        }                                       \
    }

/*
 * Immediately aborts the parsing function if EOF is encountered.
 */

#define PROPAGATE_EOF(ret)                      \
    {                                           \
        if (stream.eof())                       \
        {                                       \
            return (ret);                       \
        }                                       \
    }

/*
 * Immediately aborts the parsing function if a non-EOF I/O exception
 * is found.
 */

#define PROPAGATE_BAD(ret)                      \
    {                                           \
        if (stream.bad())                       \
        {                                       \
            return (ret);                       \
        }                                       \
    }

/*
 * Immediately aborts the parsing function if any non-success code is
 * found.  Instead of returning immediately, we jump to the [error]
 * label to allow any needed cleanup code to be run.
 */
    
#define FINALIZE_ANY_ERROR                      \
    {                                           \
        if (!stream.good())                     \
        {                                       \
            goto error;                         \
        }                                       \
    }

/*
 * Immediately aborts the parsing function if any I/O exception is
 * found.  Instead of returning immediately, we jump to the [error]
 * label to allow any needed cleanup code to be run.
 */

#define FINALIZE_IO_ERROR                       \
    {                                           \
        if (stream.eof() || stream.bad())       \
        {                                       \
            goto error;                         \
        }                                       \
    }

#endif // HST_IO_MACROS_HH
