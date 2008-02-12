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


#include <iostream>

#include <hst/equivalence.hh>

using namespace std;
using namespace hst;

int main()
{
    equivalences_t  equiv;

    string   command;
    state_t  op1, op2;

    cin >> skipws;

    cin >> command >> op1 >> op2;;
    while (!cin.fail())
    {
        if (command == "add")
        {
            equiv.add(op1, op2);
        } else if (command == "merge") {
            equiv.merge(op1, op2);
        } else {
            cerr << "Invalid command: " << command << endl;
            return 2;
        }

        cin >> command >> op1 >> op2;;
    }

    cout << equiv;

    return 0;
}
