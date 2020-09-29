{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedLists #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}
module Main (main) where
import Prelude hiding (state)

import CLI
import Types
import API

import qualified Data.Map as M

import Control.Arrow ((***))
import System.Posix.ByteString
import Data.ByteString.Char8 (unpack)
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager, ManagerSettings(..))
import Network.HTTP.Client.Internal (makeConnection)
import Network.Socket
import qualified Network.Socket.ByteString as NBS
import Servant.Client
import Servant.Client.Generic

import qualified Text.PrettyPrint.ANSI.Leijen as PP
import qualified Data.Text as T
import qualified Data.Vector.Sized as VB
import Data.Semigroup (Max(..))

get'interfaces :: ClientM Interfaces
get'interface :: InterfaceName -> ClientM Interface
get'port :: InterfaceName -> PortName -> ClientM Port
set'port'state :: InterfaceName -> PortName -> Bool -> ClientM Port
set'ports :: InterfaceName -> [(PortName, Bool)] -> ClientM Interface
Routes { _get'interfaces = get'interfaces
       , _get'interface = get'interface
       , _get'port = get'port
       , _get'port'diagnostic = _
       , _get'port'state = _
       , _set'port'state = set'port'state
       , _set'ports = set'ports
       } = genericClient

text :: Coercible a Text => a -> PP.Doc
text = PP.text . T.unpack . coerce

prettyInterface :: Interface -> PP.Doc
prettyInterface Interface{..} =
  PP.vsep [ text name PP.<+> prettyInterfaceStatus ifStatus
          , PP.indent 2 prettyPorts
          ]

  where prettyPorts = PP.vsep $ flip map (VB.toList ports) \Port{..} ->
                 PP.fill (maxLength + 1)
                   ( text name <> PP.text ":" )
          PP.<+> PP.align (prettyDiagnostic diagnostic PP.<$> prettyState state)

        maxLength = getMax $ foldMap (\Port{..} -> Max . T.length . coerce $ name) ports

prettyInterfaceStatus :: InterfaceStatus -> PP.Doc
prettyInterfaceStatus Disconnected = PP.text "disconnected"
prettyInterfaceStatus Operational = PP.text "operational"

prettyDiagnostic :: Diagnostic -> PP.Doc
prettyDiagnostic Overload = PP.text "overloaded"
prettyDiagnostic Grounded = PP.text "grounded"
prettyDiagnostic Open = PP.text "open"
prettyDiagnostic Okay = PP.text "okay"

prettyState :: Bool -> PP.Doc
prettyState True = PP.text "on"
prettyState False = PP.text "off"

prettyPort :: InterfaceName -> Port -> PP.Doc
prettyPort i Port{..} =
         text i <> PP.text "/" <> text name <> PP.text ":"
  PP.<+> PP.align (prettyDiagnostic diagnostic PP.<$> prettyState state)

prettyInterfaces :: Interfaces -> PP.Doc
prettyInterfaces (Interfaces []) = PP.empty
prettyInterfaces (Interfaces [i]) = prettyInterface i
prettyInterfaces (Interfaces (i:is)) = prettyInterface i PP.<$> prettyInterfaces (Interfaces is)

req :: Command -> ClientM ()
req (Status SAll) = do
  is <- get'interfaces
  liftIO $ PP.putDoc (prettyInterfaces is <> PP.line)

req (Status (SInterface interface)) = do
  i <- get'interface interface
  liftIO $ PP.putDoc (prettyInterface i <> PP.line)

req (Status (SFullPort interface port)) = do
  p <- get'port interface port
  liftIO $ PP.putDoc (prettyPort interface p <> PP.line)

req (On interface (port :| [])) = do
  p <- set'port'state interface port True
  liftIO $ PP.putDoc (prettyPort interface p <> PP.line)

req (On interface ports) = do
  i <- set'ports interface (toList $ (,True) <$> ports)
  liftIO $ PP.putDoc (prettyInterface i <> PP.line)

req (Off interface (port :| [])) = do
  p <- set'port'state interface port False
  liftIO $ PP.putDoc (prettyPort interface p <> PP.line)

req (Off interface ports) = do
  i <- set'ports interface (toList $ (,False) <$> ports)
  liftIO $ PP.putDoc (prettyInterface i <> PP.line)

portMapping :: PortMapping
portMapping = PortMapping . M.fromList . map (PortAbbrev *** (InterfaceName *** (PortName <$>))) $
  [ ("chlorine", ("closet", ["chlorine", "chlorine-aux"]))
  , ("rigol",    ("rack",   ["1"]))
  ]

newUnixSocketManager :: RawFilePath -> IO Manager
newUnixSocketManager p =
  newManager defaultManagerSettings { managerRawConnection = pure openSocket }
  where openSocket _ _ _ = do
          s <- socket AF_UNIX Stream 0
          connect s (SockAddrUnix $ unpack p)
          makeConnection (NBS.recv s 8096)
                         (NBS.sendAll s)
                         (close s)

main :: IO ()
main = do
  c <- cli portMapping
  mgr <- newUnixSocketManager "\0relayd"
  runClientM (req c) (mkClientEnv mgr (BaseUrl Http "localhost" 9999 ""))
    >>= either print (const $ pure ())
