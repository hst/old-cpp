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

module HST.CSPM.Tool

> module Main
>     (
>      main
>     )
>     where

> import Control.Exception
> import Data.Map (Map)
> import qualified Data.Map as Map
> import System.Environment

> import HST.CSPM.Tool.Command
> import HST.CSPM.Tool.Compile
> import HST.CSPM.Tool.Evaluate

> type Commands = Map String Command

> commands :: Commands
> commands = Map.fromList [
>             ("compile", cmdCompile),
>             ("evaluate", cmdEvaluate)
>            ]

> describeCmd :: (String, Command) -> String
> describeCmd (x, c) = "  " ++ x ++ ": " ++ (description c)

> usage_
>     = unlines $ [
>        "Usage: cspm [COMMAND] [OPTIONS]",
>        "",
>        "Available commands:"
>       ] ++ map describeCmd (Map.toList commands)

> execCmd cmdName rest
>     = do
>       case Map.lookup cmdName commands of
>         Nothing  -> ioError (userError usage_)
>         Just cmd -> (action cmd) rest

> action_
>     = do
>       args <- getArgs
>       case args of
>         []         -> ioError (userError usage_)
>         (cmd:rest) -> execCmd cmd rest
>       return ()

> main
>     = do
>       result <- tryJust userErrors action_
>       case result of
>         Right _  -> return ()
>         Left err -> putStr err
