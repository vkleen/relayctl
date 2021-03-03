module API
  ( InterfaceName (..),
    DeviceName (..),
    Interfaces (..),
    Interface (..),
    InterfaceCommand (..),
    InterfaceStatus (..),
    PortState (..),
    PortIndex (..),
    Device (..),
    Devices (..),
    Diagnostic (..),
    applyCommand
  )
where

import Data.Finite
import Data.Aeson
import Data.Aeson.Encoding (pair, text)
import Data.Aeson.Types (listValue, prependFailure, unexpected)
import qualified Data.Vector.Sized as VS
-- import Servant.API
-- import Servant.API.Generic

-- relaydApi :: Proxy (ToServantApi Routes)
-- relaydApi = genericApi (Proxy :: Proxy Routes)

-- data Routes r = Routes
--   { _get'interfaces :: r :- Get '[JSON] Interfaces,
--     _get'interface ::
--       r :- Capture "interface" InterfaceName
--         :> Get '[JSON] Interface,
--     _get'port ::
--       r :- Capture "interface" InterfaceName :> Capture "port" PortName
--         :> Get '[JSON] Port,
--     _get'port'diagnostic ::
--       r :- Capture "interface" InterfaceName :> Capture "port" PortName
--         :> "diagnostic"
--         :> Get '[JSON] Diagnostic,
--     _get'port'state ::
--       r :- Capture "interface" InterfaceName :> Capture "port" PortName
--         :> "state"
--         :> Get '[JSON] Bool,
--     _set'port'state ::
--       r :- Capture "interface" InterfaceName :> Capture "port" PortName
--         :> "state"
--         :> ReqBody '[JSON] Bool
--         :> Put '[JSON] Port,
--     _set'ports ::
--       r :- Capture "interface" InterfaceName
--         :> ReqBody '[JSON] [(PortName, Bool)]
--         :> Post '[JSON] Interface
--   }
--   deriving (Generic)

newtype InterfaceName = InterfaceName Text
  -- deriving (FromHttpApiData, ToHttpApiData) via Text
  deriving (ToJSON, FromJSON) via Text
  deriving (Show, IsString) via Text
  deriving (Generic, Eq, Ord, NFData)

newtype DeviceName = DeviceName Text
  -- deriving (FromHttpApiData, ToHttpApiData) via Text
  deriving (ToJSON, FromJSON) via Text
  deriving (Show, IsString) via Text
  deriving (Generic, Eq, Ord, NFData)

newtype Interfaces = Interfaces [Interface]
  deriving (Generic, Show, Eq, NFData)

newtype Devices = Devices [Device]
  deriving (Generic, Show, Eq, NFData)

data InterfaceStatus
  = Operational
  | Disconnected
  deriving (Generic, Show, Eq, NFData)

data Interface = Interface
  { name :: InterfaceName,
    ifStatus :: InterfaceStatus,
    ports :: VS.Vector 8 (Diagnostic, PortState)
  }
  deriving (Generic, Show, Eq, NFData)

newtype InterfaceCommand = InterfaceCommand (VS.Vector 8 PortState)
  deriving (Generic, Show, Eq, NFData)

applyCommand :: InterfaceCommand -> Interface -> Interface
applyCommand (InterfaceCommand cmd) ai@Interface { ports = ps' } =
  ai { ports = VS.zipWith (\o c -> (fst o, c)) ps' cmd }

data Diagnostic
  = Overload
  | Grounded
  | Open
  | Okay
  | Unknown
  deriving (Generic, Show, Eq, Ord, Enum, NFData)

data PortState
  = On
  | Off
  deriving (Generic, Show, Eq, Ord, Enum, NFData)

data Device = Device
  { name :: DeviceName
  , devicePorts :: [(InterfaceName, Finite 8)]
  }
  deriving (Generic, Show, Eq, NFData)

newtype PortIndex = PortIndex (Finite 8)
  deriving (Show, Eq) via Finite 8
  deriving (Generic, NFData)

instance ToJSON Diagnostic where
  toJSON Overload = String "Overload"
  toJSON Grounded = String "Grounded"
  toJSON Open = String "Open"
  toJSON Okay = String "Okay"
  toJSON Unknown = String "Unknown"

  toEncoding Overload = text "Overload"
  toEncoding Grounded = text "Grounded"
  toEncoding Open = text "Open"
  toEncoding Okay = text "Okay"
  toEncoding Unknown = text "Unknown"

instance FromJSON Diagnostic where
  parseJSON = withText "Diagnostic" \case
    x
      | x == "Overload" -> pure Overload
      | x == "Grounded" -> pure Grounded
      | x == "Open" -> pure Open
      | x == "Okay" -> pure Okay
      | x == "Unknown" -> pure Unknown
      | otherwise -> prependFailure "parsing Diagnostic failed, " (unexpected $ String x)

instance ToJSON PortState where
  toJSON On = String "on"
  toJSON Off = String "off"

  toEncoding On = text "on"
  toEncoding Off = text "off"

instance FromJSON PortState where
  parseJSON = withText "PortState" \case
    x
      | x == "on" -> pure On
      | x == "off" -> pure Off
      | otherwise -> prependFailure "parsing PortState failed, " (unexpected $ String x)

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
  toJSON (Interfaces is) = listValue toJSON is
  toEncoding (Interfaces is) = foldable is

deriving via [Interface] instance FromJSON Interfaces

instance ToJSON Devices where
  toJSON (Devices ds) = listValue toJSON ds
  toEncoding (Devices ds) = foldable ds

deriving via [Device] instance FromJSON Devices

instance ToJSON PortIndex where
  toJSON = toJSON . getFinite . coerce
  toEncoding = toEncoding . getFinite . coerce

instance FromJSON PortIndex where
  parseJSON = parseJSON @Integer >=>
    ( packFinite @8 >>>
      maybe (fail "PortIndex must be between 0 and 7 inclusive") (pure . coerce)
    )

instance ToJSON Device where
  toJSON (Device n ps) =
    object [ "name" .= n
           , "ports" .= listValue toJSON (coerce @_ @[(InterfaceName, PortIndex)] ps)
           ]
  toEncoding (Device n ps) =
    pairs $ "name" .= n <>
            pair "ports" (foldable $ coerce @_ @[(InterfaceName, PortIndex)] ps)

instance FromJSON Device where
  parseJSON = withObject "Device" \p ->
    Device
      <$> p .: "name"
      <*> (coerce @[(InterfaceName, PortIndex)] <$> p .: "ports")

instance ToJSON Interface where
  toJSON (Interface n s ps) =
    object ["name" .= n, "state" .= s, "ports" .= listValue toJSON (toList ps)]
  toEncoding (Interface n s ps) =
    pairs $ "name" .= n <> "state" .= s <> pair "ports" (foldable ps)

instance FromJSON Interface where
  parseJSON = withObject "Interface" \i ->
    Interface
      <$> i .: "name"
      <*> i .: "state"
      <*> i .: "ports"

instance FromJSON (VS.Vector 8 (Diagnostic, PortState)) where
  parseJSON = withArray "Vec 8" $ \v ->
    let mvv = VS.toSized v
    in case mvv of
         Just vv -> traverse parseJSON vv
         Nothing -> fail "Expected exactly 8 entries in array"

instance ToJSON (VS.Vector 8 (Diagnostic, PortState)) where
  toJSON = listValue toJSON . toList
  toEncoding = foldable

instance ToJSON InterfaceCommand where
  toJSON = listValue toJSON . toList . coerce @_ @(VS.Vector 8 PortState)
  toEncoding = foldable . coerce @_ @(VS.Vector 8 PortState)

instance FromJSON InterfaceCommand where
  parseJSON = withArray "Vec 8" $ \v ->
    let mvv = VS.toSized v
    in case mvv of
         Just vv -> InterfaceCommand <$> traverse parseJSON vv
         Nothing -> fail "Expected exactly 8 entries in array"
