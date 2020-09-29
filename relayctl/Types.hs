module Types where

import qualified Data.Map as M
import API

data RelayCtlError = NoSuchPortAbbrev PortAbbrev
                   | NoSuchPort InterfaceName PortName
                   | NoSuchInterface InterfaceName
  deriving Show

instance Exception RelayCtlError

newtype PortAbbrev = PortAbbrev Text
  deriving (Eq, Ord) via Text
  deriving Show

newtype PortMapping = PortMapping (M.Map PortAbbrev (InterfaceName, NonEmpty PortName))

unabbrev :: PortMapping -> PortAbbrev -> Maybe (InterfaceName, NonEmpty PortName)
unabbrev (PortMapping ps) a = ps M.!? a
