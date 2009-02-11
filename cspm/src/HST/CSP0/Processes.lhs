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

> module HST.CSP0.Processes where

> import Control.Monad.State
> import Data.Set (Set)
> import qualified Data.Set as Set


Events and processes

Event and process identifiers are represented as Strings.  They should
only consist of those characters allowed in a CSP₀ identifier, though
this isn't checked.

> newtype Event = Event String
>     deriving (Eq, Ord)

> newtype Alphabet = Alphabet (Set Event)
>     deriving (Eq, Ord)

> data EventPair = EventPair Event Event
>     deriving (Eq, Ord)

> newtype EventMap = EventMap (Set EventPair)
>     deriving (Eq, Ord)

> newtype Process = Process String
>     deriving (Eq, Ord)

> newtype ProcessSet = ProcessSet (Set Process)
>     deriving (Eq, Ord)

Predefined processes

> stop :: Process
> stop = Process "STOP"

> skip :: Process
> skip = Process "SKIP"

We don't want to show the [] brackets when showing a list of events,
event pairs, or processes, since we're going to use different brackets
depending on whether the list represents a sequence, set, or tuple
literal.

> instance Show Event where
>     show (Event a) = a
>
>     showList []     = showString ""
>     showList (x:xs) = shows x . showl xs
>         where
>           showl []     = id
>           showl (x:xs) = showChar ',' . shows x . showl xs

> instance Show EventPair where
>     show (EventPair a b) = show a ++ "->" ++ show b
>
>     showList []     = showString ""
>     showList (x:xs) = shows x . showl xs
>         where
>           showl []     = id
>           showl (x:xs) = showString ", " . shows x . showl xs

> instance Show Process where
>     show (Process p) = p
>
>     showList []     = showString ""
>     showList (x:xs) = shows x . showl xs
>         where
>           showl []     = id
>           showl (x:xs) = showChar ',' . shows x . showl xs

> instance Show Alphabet where
>     show (Alphabet a) = "{" ++ show (Set.toList a) ++ "}"

> instance Show EventMap where
>     show (EventMap m) = "[[ " ++ show (Set.toList m) ++ " ]]"

> instance Show ProcessSet where
>     show (ProcessSet ps) = "{" ++ show (Set.toList ps) ++ "}"


CSP₀ statements

The Statement type defines all of the possible CSP₀ statements.  Most
of them are defined as record types, allowing you to access the
statement's operands easily.

> data Statement
>     = SProcess Process
>     | SEvent Event
>     | SAlias {
>         dest :: Process,
>         p    :: Process
>       }
>     | SPrefix {
>         dest :: Process,
>         a    :: Event,
>         p    :: Process
>       }
>     | SExtChoice {
>         dest :: Process,
>         p    :: Process,
>         q    :: Process
>       }
>     | SIntChoice {
>         dest :: Process,
>         p    :: Process,
>         q    :: Process
>       }
>     | STimeout {
>         dest :: Process,
>         p    :: Process,
>         q    :: Process
>       }
>     | SSeqComp {
>         dest :: Process,
>         p    :: Process,
>         q    :: Process
>       }
>     | SInterleave {
>         dest :: Process,
>         p    :: Process,
>         q    :: Process
>       }
>     | SIParallel {
>         dest  :: Process,
>         p     :: Process,
>         alpha :: Alphabet,
>         q     :: Process
>       }
>     | SAParallel {
>         dest  :: Process,
>         p     :: Process,
>         alpha :: Alphabet,
>         beta  :: Alphabet,
>         q     :: Process
>       }
>     | SHide {
>         dest  :: Process,
>         p     :: Process,
>         alpha :: Alphabet
>       }
>     | SRename {
>         dest :: Process,
>         p    :: Process,
>         emap :: EventMap
>       }
>     | SRExtChoice {
>         dest :: Process,
>         ps   :: ProcessSet
>       }
>     | SRIntChoice {
>         dest :: Process,
>         ps   :: ProcessSet
>       }


The Statement type's Show instance outputs the correct CSP₀ syntax for
the statement.

> instance Show Statement where
>     show (SProcess p) = "process " ++ show p ++ ";"
>     show (SEvent a) = "event " ++ show a ++ ";"
>     show (SAlias dest p) = "alias " ++ show dest ++ " = " ++ show p ++ ";"
>     show (SPrefix dest a p) = "prefix " ++ show dest ++ " = " ++
>                               show a ++ " -> " ++
>                               show p ++ ";"
>     show (SExtChoice dest p q) = "extchoice " ++ show dest ++ " = " ++
>                                  show p ++ " [] " ++ show q ++ ";"
>     show (SIntChoice dest p q) = "intchoice " ++ show dest ++ " = " ++
>                                  show p ++ " |~| " ++ show q ++ ";"
>     show (STimeout dest p q) = "timeout " ++ show dest ++ " = " ++
>                                show p ++ " [> " ++ show q ++ ";"
>     show (SSeqComp dest p q) = "seqcomp " ++ show dest ++ " = " ++
>                                show p ++ " ; " ++ show q ++ ";"
>     show (SInterleave dest p q) = "interleave " ++ show dest ++ " = " ++
>                                   show p ++ " ||| " ++ show q ++ ";"
>     show (SIParallel dest p alpha q)
>         = "iparallel " ++ show dest ++ " = " ++ show p ++ " [|" ++
>           show alpha ++ "|] " ++ show q ++ ";"
>     show (SAParallel dest p alpha beta q)
>         = "aparallel " ++ show dest ++ " = " ++ show p ++ " [" ++
>           show alpha ++ "||" ++ show beta ++ "] " ++ show q ++ ";"
>     show (SHide dest p alpha) = "hide " ++ show dest ++ " = " ++ show p ++
>                                 " \\ " ++ show alpha ++ ";"
>     show (SRename dest p emap) = "rename " ++ show dest ++ " = " ++ show p ++
>                                  " " ++ show emap ++ ";"
>     show (SRExtChoice dest ps) = "rextchoice " ++ show dest ++ " = [] " ++
>                                  show ps ++ ";"
>     show (SRIntChoice dest ps) = "rintchoice " ++ show dest ++ " = |~| " ++
>                                  show ps ++ ";"


Return the process name created by a statement.  Note that the
“process” and “event” statements do not define any processes.

> definedProcess :: Statement -> Maybe Process
> definedProcess (SAlias { dest = dest }) = Just dest
> definedProcess (SPrefix { dest = dest }) = Just dest
> definedProcess (SExtChoice { dest = dest }) = Just dest
> definedProcess (SIntChoice { dest = dest }) = Just dest
> definedProcess (STimeout { dest = dest }) = Just dest
> definedProcess (SSeqComp { dest = dest }) = Just dest
> definedProcess (SInterleave { dest = dest }) = Just dest
> definedProcess (SIParallel { dest = dest }) = Just dest
> definedProcess (SAParallel { dest = dest }) = Just dest
> definedProcess (SHide { dest = dest }) = Just dest
> definedProcess (SRename { dest = dest }) = Just dest
> definedProcess (SRExtChoice { dest = dest }) = Just dest
> definedProcess (SRIntChoice { dest = dest }) = Just dest
> definedProcess _ = Nothing


Create a new event name that does not yet exist in the specified
alphabet.  You specify a desired name; if that name is already taken
then we append integer suffixes until we find an unused name.

> uniqueEventName :: Event -> Alphabet -> Event
> uniqueEventName desired taken
>     = head $ dropWhile (flip Set.member set) names
>       where
>         Event prefix = desired
>         Alphabet set = taken
>         names = desired : map addSuffix [0..]
>         addSuffix suffix = Event (prefix ++ "." ++ show suffix)


Create a new process name that does not yet exist in the specified
process set.  You specify a desired name; if that name is already
taken then we append integer suffixes until we find an unused name.

> uniqueProcessName :: Process -> ProcessSet -> Process
> uniqueProcessName desired taken
>     = head $ dropWhile (flip Set.member set) names
>       where
>         Process prefix = desired
>         ProcessSet set = taken
>         names = desired : map addSuffix [0..]
>         addSuffix suffix = Process (prefix ++ "." ++ show suffix)


A Script is a list of CSP₀ statements.  We also keep track of the
processes and events declared in the script (using “process” and
“event” statements).  The “pending” and “defined” sets are used by the
defineProcess function to ensure that we don't try to define any
process more than once.

> data Script
>     = Script {
>         processes  :: ProcessSet,
>         events     :: Alphabet,
>         statements :: [Statement]
>       }
>     deriving Show

> instance HasAlphabet Script where
>     getAlphabet = events
>     putAlphabet s a = s { events = a }

> instance HasProcessSet Script where
>     getProcessSet = processes
>     putProcessSet s ps = s { processes = ps }

> type ScriptTransformer a = State Script a


> outputScript s = unlines $ map show $ statements s


The empty CSP₀ script.

> emptyScript :: Script
> emptyScript = Script {
>                 processes  = ProcessSet Set.empty,
>                 events     = Alphabet Set.empty,
>                 statements = []
>               }


Applies a Script transformer to the empty Script, returning the
result.

> createScript :: (ScriptTransformer a) -> Script
> createScript creator = execState creator emptyScript


Adding statements to a script

The addStatement function creates a Script transformer that adds a
Statement to a Script.

> addStatement :: Statement -> ScriptTransformer ()

> addStatement stmt
>     = do
>       s <- get
>       put $ s { statements = statements s ++ [stmt] }


We provide helper functions that call addStatement for each kind of
CSP₀ statement.

> process :: Process -> ScriptTransformer ()
> process p = addStatement (SProcess p)

> event :: Event -> ScriptTransformer ()
> event a = addStatement (SEvent a)

> alias :: Process -> Process -> ScriptTransformer ()
> alias dest p = addStatement (SAlias dest p)

> prefix :: Process -> Event -> Process -> ScriptTransformer ()
> prefix dest a p = addStatement (SPrefix dest a p)

> extchoice :: Process -> Process -> Process -> ScriptTransformer ()
> extchoice dest p q = addStatement (SExtChoice dest p q)

> intchoice :: Process -> Process -> Process -> ScriptTransformer ()
> intchoice dest p q = addStatement (SIntChoice dest p q)

> timeout :: Process -> Process -> Process -> ScriptTransformer ()
> timeout dest p q = addStatement (STimeout dest p q)

> seqcomp :: Process -> Process -> Process -> ScriptTransformer ()
> seqcomp dest p q = addStatement (SSeqComp dest p q)

> interleave :: Process -> Process -> Process -> ScriptTransformer ()
> interleave dest p q = addStatement (SInterleave dest p q)

> iparallel :: Process -> Process -> Alphabet -> Process -> ScriptTransformer ()
> iparallel dest p alpha q = addStatement (SIParallel dest p alpha q)

> aparallel :: Process -> Process -> Alphabet -> Alphabet -> Process ->
>              ScriptTransformer ()
> aparallel dest p alpha beta q = addStatement (SAParallel dest p alpha beta q)

> hide :: Process -> Process -> Alphabet -> ScriptTransformer ()
> hide dest p alpha = addStatement (SHide dest p alpha)

> rename :: Process -> Process -> EventMap -> ScriptTransformer ()
> rename dest p emap = addStatement (SRename dest p emap)

> rextchoice :: Process -> ProcessSet -> ScriptTransformer ()
> rextchoice dest ps = addStatement (SRExtChoice dest ps)

> rintchoice :: Process -> ProcessSet -> ScriptTransformer ()
> rintchoice dest ps = addStatement (SRIntChoice dest ps)


A Script State transformer that creates a new, unique event name.

> class HasAlphabet a where
>     getAlphabet :: a -> Alphabet
>     putAlphabet :: a -> Alphabet -> a

> newEvent :: (HasAlphabet a) => String -> State a Event
> newEvent prefix
>     = do
>       s <- get
>       let alphabet = getAlphabet s
>           Alphabet set = alphabet
>           newEvent = uniqueEventName (Event prefix) alphabet
>       put $ putAlphabet s $ Alphabet $ Set.insert newEvent set
>       return newEvent


A Script State transformer that creates a new, unique process name.

> class HasProcessSet a where
>     getProcessSet :: a -> ProcessSet
>     putProcessSet :: a -> ProcessSet -> a

> newProcess :: (HasProcessSet a) => String -> State a Process
> newProcess prefix
>     = do
>       s <- get
>       let procSet = getProcessSet s
>           ProcessSet set = procSet
>           newProcess = uniqueProcessName (Process prefix) procSet
>       put $ putProcessSet s $ ProcessSet $ Set.insert newProcess set
>       return newProcess


A Script State transformer that ensures that a specified event has
been declared.

> defineEvent :: Event -> ScriptTransformer ()
> defineEvent e
>     = do
>       s <- get
>       let Alphabet set = events s
>       when (e `Set.notMember` set) $ do
>         put $ putAlphabet s $ Alphabet $ Set.insert e set
>         event e


The defineProcess function allows you to ensure that you only define a
process once in a Script.  You provide the Process name and a State
transformer that will create the process when executed.  If the
process has not yet been defined, we add the process to the “pending”
set and execute the definer.  Once the definer finishes, we add the
process to the “defined” set.  If the definer happens to refer to
process recursively (and thereby try to define it again), then the
definer won't be executed again.

> needsDefining :: (HasProcessSet a) => Process -> State a Bool
> needsDefining p
>     = do
>       s <- get
>       let ProcessSet set = getProcessSet s
>       return $ p `Set.notMember` set

> startDefining :: (HasProcessSet a) => Process -> State a ()
> startDefining p
>     = do
>       s <- get
>       let ProcessSet set = getProcessSet s
>       put $ putProcessSet s $ ProcessSet $ Set.insert p set

> defineProcess :: (HasProcessSet a) => Process -> State a b -> State a ()
> defineProcess dest definer
>     = do
>       needed <- needsDefining dest
>       when needed $ do
>                     startDefining dest
>                     definer
>                     return ()
