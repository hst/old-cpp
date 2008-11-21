-----------------------------------------------------------------------
  Copyright © 2008 Douglas Creager

    This library is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later
    version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free
    Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
    MA 02111-1307 USA
------------------------------------------------------------------------

> module HST.CSPM.Definitions where

> import Data.List
> import Data.Maybe

> import HST.CSP0
> import HST.CSPM.Types
> import HST.CSPM.Environments
> import HST.CSPM.Patterns


Create a list of Bindings (and the appropriate Expressions inside of
them) for a single definition.  We don't create any bindings for
individual DLambdaClauses, since those will have been merged into
DLambdas before this function is called.  Pattern definitions are
currently the only definitions that can create multiple bindings.

> createBinding :: Definition -> [Binding]

> createBinding (DPatternDefn p x)
>     = map binder (patternIds p)
>     where
>       binder id = Binding id $ EExtractMatch id p x

> createBinding (DLambda id cs)
>     = [Binding id $ ELambda cs]

> createBinding (DSimpleChannel id)
>     = [Binding id (EEvent (Event id'))]
>     where
>       Identifier id' = id


Create a list of Bindings for a list of Definitions.

> createBindings :: [Definition] -> [Binding]
> createBindings = concatMap createBinding


Find instances where a function is defined using multiple branches
(each represented by a DLambdaClause), and merge them together into a
single DLambda definition.  First we use the groupBy function to find
consecutive DLambdaClauses (that define the same identifier), and then
use the merge and extract helper functions to create the new DLambdas.

> mergeLambdas :: [Definition] -> [Definition]
> mergeLambdas ds = concatMap merge (groupBy (=:=) ds)
>     where
>       (DLambdaClause id1 _) =:= (DLambdaClause id2 _) = id1 == id2
>       _ =:= _ = False
>       merge dlcs@((DLambdaClause id _):_) = [DLambda id $ extract dlcs]
>       merge ds = ds
>       extract ((DLambdaClause _ lc):dlcs) = lc : extract dlcs
>       extract [] = []


To create a root Environment for a list of Definitions, we first merge
together all of the DLambdaClauses into DLambdas, and then use
createBindings to create the contents of the new Environment.

> createRootEnv :: CSPMScript -> Env
> createRootEnv (CSPMScript defs) = rootEnv $ createBindings $ mergeLambdas defs


To create a nested Environment for a list of Definitions, we follow
the same process as above; we just use extendEnv instead of rootEnv to
create the Environment.

> createNestedEnv :: String -> Env -> [Definition] -> Env
> createNestedEnv name e defs
>     = extendEnv name e $ createBindings $ mergeLambdas defs