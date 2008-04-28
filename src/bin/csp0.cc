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

#ifndef CSP0_CC
#define CSP0_CC

#include <cstring>
#include <fstream>
#include <iostream>

#include <tr1/memory>

#include <hst/csp.hh>
#include <hst/assertions.hh>

using namespace std;
using namespace hst;

/*
 * Many of the commands can operate on an ifstream or on standard
 * input.  The ifstream should be deleted after we're done, whereas
 * cin should not.  So this is a simple “destructor” object for use
 * with the shared_ptrs that doesn't actually free the object.
 */

template<typename T>
struct NullDestructor
{
    void operator () (T *ptr)
    {
    }
};

NullDestructor<istream>  istream_destructor;

/*
 * List the available commands.
 */

void print_usage(ostream &stream)
{
    stream << "csp0 [command]" << endl
           << endl
           << "Commands:" << endl
           << "  help    Help on using this utility" << endl
           << "  lts     Print out the LTS for a CSP0 script" << endl
           << "  assert  Test assertions on a CSP0 script" << endl
        ;
}

/*
 * Describe each of the commands.
 */

void print_help_usage(ostream &stream)
{
    stream << "csp0 help [command]" << endl
           << endl
           << "Prints out usage information for the specified command." << endl
        ;
}

void print_lts_usage(ostream &stream)
{
    stream << "csp0 lts [script file]" << endl
           << endl
           << "Prints out the LTS for a CSP0 script." << endl
        ;
}

void print_assert_usage(ostream &stream)
{
    stream << "csp0 assert [script file] [assertions]" << endl
           << endl
           << "Tests assertions on a CSP0 script.  Each assertion should" << endl
           << "be of the following form:" << endl
           << endl
           << "    --traces-refinement [spec] [impl]" << endl
        ;
}

/*
 * The “help” command describes each of the commands.
 */

int do_help(int argc, const char **argv)
{
    if (argc != 1)
    {
        print_help_usage(cerr);
        return 1;
    }

    // Extract the command name

    const char *cmd = *argv++;
    --argc;

    if (strcmp(cmd, "help") == 0)
        print_help_usage(cout);
    else if (strcmp(cmd, "lts") == 0)
        print_lts_usage(cout);
    else if (strcmp(cmd, "assert") == 0)
        print_assert_usage(cout);
    else {
        cerr << "Unknown command " << cmd << endl;
        return 1;
    }

    return 0;
}

/*
 * The “lts” command prints out the LTS for a CSP0 script.
 */

int do_lts(int argc, const char **argv)
{
    if (argc != 1)
    {
        print_lts_usage(cerr);
        return 1;
    }

    // Extract the filename

    const char *filename = *argv++;
    --argc;

    // Open a stream for the filename; either an ifstream if it's a
    // real filename, or cin if it's a “-”.  Since it's a shared_ptr,
    // it will get deleted (and therefore closed) once it goes out of
    // scope.

    shared_ptr<istream>  in;

    if (strcmp(filename, "-") == 0)
    {
        in.reset(&cin, istream_destructor);
    } else {
        in.reset(new ifstream(filename));
    }

    // Try to read the CSP0 script from the specified file.

    csp_t  csp;

    read_csp0(*in, csp);

    if (in->fail())
        return 2;

    // Output the CSP script's LTS.

    cout << csp;
    return 0;
}

/*
 * The “assert” command tests assertions on a CSP0 script.
 */

int do_assert(int argc, const char **argv)
{
    if (argc < 1)
    {
        // Not enough arguments, we at least need a filename.
        print_assert_usage(cerr);
        return 1;
    }

    // Extract the filename

    const char *filename = *argv++;
    --argc;

    // Open a stream for the filename; either an ifstream if it's a
    // real filename, or cin if it's a “-”.  Since it's a shared_ptr,
    // it will get deleted (and therefore closed) once it goes out of
    // scope.

    shared_ptr<istream>  in;

    if (strcmp(filename, "-") == 0)
    {
        in.reset(&cin, istream_destructor);
    } else {
        in.reset(new ifstream(filename));
    }

    // Try to read the CSP0 script from the specified file.

    csp_t  csp;

    read_csp0(*in, csp);

    if (in->fail())
        return 2;

    // Try to parse the remaining command-line arguments as
    // assertions.

    while (argc > 0)
    {
        const char  *flag = *argv++;
        --argc;

        if (strcmp(flag, "--traces-refinement") == 0)
        {
            // We need two process names

            if (argc < 2)
            {
                cerr << "Need two processes for a traces refinement!"
                     << endl;
                return 2;
            }

            const char  *spec_name = *argv++;
            const char  *impl_name = *argv++;
            argc -= 2;

            // Look up the process names in the CSP script.

            state_t  spec, impl;

            spec = csp.get_process(spec_name);
            if (spec == HST_ERROR_STATE)
            {
                cerr << "Specification process "
                     << spec_name << " does not exist."
                     << endl;
                return 3;
            }

            impl = csp.get_process(impl_name);
            if (impl == HST_ERROR_STATE)
            {
                cerr << "Implementation process "
                     << impl_name << " does not exist."
                     << endl;
                return 3;
            }

            // Normalize the spec process.

            csp.normalized_lts()->clear(TRACES);
            csp.normalized_lts()->prenormalize(spec);
            csp.normalized_lts()->normalize();
            spec = csp.normalized_lts()->initial_normal_state(spec);

            // Check the refinement.

            trace_counterexample_t  counter;
            bool  result = refines(counter, 
                                   *csp.normalized_lts(), spec,
                                   *csp.lts(), impl);

            cout << spec_name << " [T= " << impl_name
                 << ": " << (result? "true": "false")
                 << endl;

            if (!result)
            {
                cout << "After trace <";

                trace_t::const_iterator  tit;
                bool  first = true;

                for (tit = counter.trace.begin();
                     tit != counter.trace.end();
                     ++tit)
                {
                    event_t  event = *tit;

                    if (event != csp.tau())
                    {
                        if (first)
                            first = false;
                        else
                            cout << ",";

                        cout << csp.lts()->get_event_name(*tit);
                    }
                }

                cout << ">," << endl
                     << impl_name << " can execute "
                     << csp.lts()->get_event_name(counter.event)
                     << " but " << spec_name << " cannot."
                     << endl;
            }

        } else {
            cerr << "Unknown assertion flag " << flag << endl;
            return 2;
        }
    }

    return 0;
}

int main(int argc, const char **argv)
{
    if (argc <= 1)
    {
        print_usage(cerr);
        return 1;
    }

    // Skip past the command name
    --argc;
    ++argv;

    // Extract the command name

    const char *cmd = *argv++;
    --argc;

    // Dispatch to the command implementation

    if (strcmp(cmd, "help") == 0)
        return do_help(argc, argv);
    else if (strcmp(cmd, "lts") == 0)
        return do_lts(argc, argv);
    else if (strcmp(cmd, "assert") == 0)
        return do_assert(argc, argv);
    else {
        cerr << "Unknown command " << cmd << endl;
        print_usage(cerr);
        return 1;
    }
}

#endif // CSP0_CC
