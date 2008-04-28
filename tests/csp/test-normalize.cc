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


/*
 * Reads a CSP0 script from standard input, and tries to construct an
 * internal CSP representation from it.  Then, prenormalizes the CSP's
 * LTS in terms of the process named P, and prints out the
 * prenormalized LTS.
 */

#include <iostream>

#include <hst/normalized-lts.hh>
#include <hst/csp.hh>

using namespace std;
using namespace hst;

static
void usage()
{
    cerr << "Usage: test-normalize [model]" << endl
         << "  model = T, F, or N" << endl;
}

int main(int argc, const char **argv)
{
    if (argc != 2)
    {
        usage();
        return 1;
    }

    semantic_model_t  semantic_model;

    switch (argv[1][0])
    {
      case 'T':
        semantic_model = TRACES;
        break;

      case 'F':
        semantic_model = FAILURES;
        break;

      case 'N':
        semantic_model = FAILURES_DIVERGENCES;
        break;

      default:
        usage();
        return 2;
    }

    csp_t  csp;

    read_csp0(cin, csp);

    if (!cin.fail())
    {
        csp.normalized_lts()->clear(semantic_model);

        state_t  P;

        P = csp.get_process("P");
        if (P != HST_ERROR_STATE)
            csp.normalized_lts()->prenormalize(P);

        P = csp.get_process("Q");
        if (P != HST_ERROR_STATE)
            csp.normalized_lts()->prenormalize(P);

        P = csp.get_process("R");
        if (P != HST_ERROR_STATE)
            csp.normalized_lts()->prenormalize(P);

        csp.normalized_lts()->normalize();

        cout << *csp.normalized_lts();
    }
}
