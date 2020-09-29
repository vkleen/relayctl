module API
  ( relaydApi,
    Routes (..),
    InterfaceName (..),
    PortName (..),
    Interfaces (..),
    Interface (..),
    InterfaceStatus (..),
    Port (..),
    Diagnostic (..),
  )
where

import Data.Aeson
import Data.Aeson.Encoding (pair, text)
import Data.Aeson.Types (listValue, prependFailure, unexpected)
import qualified Data.Vector.Sized as VS
import Servant.API
import Servant.API.Generic

relaydApi :: Proxy (ToServantApi Routes)
relaydApi = genericApi (Proxy :: Proxy Routes)

data Routes r = Routes
  { _get'interfaces :: r :- Get '[JSON] Interfaces,
    _get'interface ::
      r :- Capture "interface" InterfaceName
        :> Get '[JSON] Interface,
    _get'port ::
      r :- Capture "interface" InterfaceName :> Capture "port" PortName
        :> Get '[JSON] Port,
    _get'port'diagnostic ::
      r :- Capture "interface" InterfaceName :> Capture "port" PortName
        :> "diagnostic"
        :> Get '[JSON] Diagnostic,
    _get'port'state ::
      r :- Capture "interface" InterfaceName :> Capture "port" PortName
        :> "state"
        :> Get '[JSON] Bool,
    _set'port'state ::
      r :- Capture "interface" InterfaceName :> Capture "port" PortName
        :> "state"
        :> ReqBody '[JSON] Bool
        :> Put '[JSON] Port,
    _set'ports ::
      r :- Capture "interface" InterfaceName
        :> ReqBody '[JSON] [(PortName, Bool)]
        :> Post '[JSON] Interface
  }
  deriving (Generic)

newtype InterfaceName = InterfaceName Text
  deriving (FromHttpApiData, ToHttpApiData) via Text
  deriving (ToJSON, FromJSON) via Text
  deriving (Show, IsString) via Text
  deriving (Generic, Eq, Ord)

newtype PortName = PortName Text
  deriving (FromHttpApiData, ToHttpApiData) via Text
  deriving (ToJSON, FromJSON) via Text
  deriving (Show, IsString) via Text
  deriving (Generic, Eq, Ord)

newtype Interfaces = Interfaces [Interface]
  deriving (Show, Eq)

data InterfaceStatus
  = Operational
  | Disconnected
  deriving (Show, Eq)

data Interface = Interface
  { name :: InterfaceName,
    ifStatus :: InterfaceStatus,
    ports :: VS.Vector 8 Port
  }
  deriving (Show, Eq)

data Diagnostic
  = Overload
  | Grounded
  | Open
  | Okay
  deriving (Show, Eq, Ord, Enum)

data Port = Port
  { name :: PortName,
    diagnostic :: Diagnostic,
    state :: Bool
  }
  deriving (Show, Eq)

instance ToJSON Diagnostic where
  toJSON Overload = String "Overload"
  toJSON Grounded = String "Grounded"
  toJSON Open = String "Open"
  toJSON Okay = String "Okay"

  toEncoding Overload = text "Overload"
  toEncoding Grounded = text "Grounded"
  toEncoding Open = text "Open"
  toEncoding Okay = text "Okay"

instance FromJSON Diagnostic where
  parseJSON = withText "Diagnostic" \case
    x
      | x == "Overload" -> pure Overload
      | x == "Grounded" -> pure Grounded
      | x == "Open" -> pure Open
      | x == "Okay" -> pure Okay
      | otherwise -> prependFailure "parsing Diagnostic failed, " (unexpected $ String x)

instance ToJSON InterfaceStatus where
  toJSON Disconnected = String "Disconnected"
  toJSON Operational = String "Operational"

  toEncoding Disconnected = text "Disconnected"
  toEncoding Operational = text "Operational"

instance FromJSON InterfaceStatus where
  parseJSON = withText "InterfaceStatus" \case
    x
      | x == "Disconnected" -> pure Disconnected
      | x == "Operational" -> pure Operational
      | otherwise -> prependFailure "parsing InterfaceStatus failed, " (unexpected $ String x)

instance ToJSON Interfaces where
  toJSON (Interfaces is) =
    listValue toJSON is
  toEncoding (Interfaces is) =
    foldable is

deriving via [Interface] instance FromJSON Interfaces

instance ToJSON Port where
  toJSON (Port n d s) =
    object ["name" .= n, "diagnostic" .= d, "on" .= s]
  toEncoding (Port n d s) =
    pairs $ "name" .= n <> "diagnostic" .= d <> "on" .= s

instance FromJSON Port where
  parseJSON = withObject "Port" \p ->
    Port
      <$> p .: "name"
      <*> p .: "diagnostic"
      <*> p .: "on"

instance ToJSON Interface where
  toJSON (Interface n s ps) =
    object $ ["name" .= n, "state" .= s, "ports" .= listValue toJSON (toList ps)]
  toEncoding (Interface n s ps) =
    pairs $ "name" .= n <> "state" .= s <> pair "ports" (foldable ps)

instance FromJSON Interface where
  parseJSON = withObject "Interface" \i ->
    Interface
      <$> i .: "name"
      <*> i .: "state"
      <*> i .: "ports"

instance FromJSON (VS.Vector 8 Port) where
  parseJSON = withArray "Vec 8" $ \v ->
    let mvv = VS.toSized v
     in case mvv of
          Just vv -> traverse parseJSON vv
          Nothing -> fail "Expected exactly 8 entries in array"
