{-# OPTIONS_GHC -Wno-orphans #-}

module CLI
  ( cli,
    Command (..),
    StatusArgs (..),
  )
where

import API
import Control.Exception
import qualified Data.Map as M
import qualified Data.Text as T
import Options.Applicative
import Options.Applicative.Help.Chunk
import qualified Text.PrettyPrint.ANSI.Leijen as PP
import Types

data StatusArgs
  = SAll
  | SInterface InterfaceName
  | SFullPort InterfaceName PortName
  deriving (Show)

data Command
  = Status StatusArgs
  | On InterfaceName (NonEmpty PortName)
  | Off InterfaceName (NonEmpty PortName)
  deriving (Show)

data RawCommand
  = RawStatus (Maybe InterfaceName) (Maybe Text)
  | RawOn (Maybe InterfaceName) Text
  | RawOff (Maybe InterfaceName) Text

cliCommand :: Parser RawCommand
cliCommand =
  hsubparser $
    command "status" (info statusOptions (progDesc "Get interface/port status"))
      <> command "on" (info onOptions (progDesc "Turn port on"))
      <> command "off" (info offOptions (progDesc "Turn port off"))
  where
    statusOptions =
      RawStatus
        <$> (optional . (InterfaceName <$>))
          ( strOption
              ( long "interface"
                  <> short 'i'
                  <> metavar "INTERFACE-NAME"
                  <> help "Show status only for this interface"
              )
          )
        <*> optional
          ( strArgument
              ( metavar "PORT-NAME"
                  <> help "Show status only for this port"
              )
          )
    onOptions =
      RawOn
        <$> (optional . (InterfaceName <$>))
          ( strOption
              ( long "interface"
                  <> short 'i'
                  <> metavar "INTERFACE-NAME"
                  <> help "Specify an explicit interface here"
              )
          )
        <*> ( strArgument
                ( metavar "PORT-NAME"
                    <> help "Turn on this port"
                )
            )
    offOptions =
      RawOff
        <$> (optional . (InterfaceName <$>))
          ( strOption
              ( long "interface"
                  <> short 'i'
                  <> metavar "INTERFACE-NAME"
                  <> help "Specify an explicit interface here"
              )
          )
        <*> ( strArgument
                ( metavar "PORT-NAME"
                    <> help "Turn on this port"
                )
            )

cli :: PortMapping -> IO Command
cli portMapping@(PortMapping ps) = do
  raw <-
    customExecParser prefs' $
      ( info
          (cliCommand <**> helper)
          ( fullDesc
              <> header "relayctl -- the client for relayd"
          )
      )
        { infoProgDesc = desc
        }
  postProcess raw
  where
    prefs' = prefs $ showHelpOnEmpty
    desc =
      stringChunk "Control relays via API calls to relayd."
        <<+>> if M.null ps
          then mempty
          else stringChunk "The <on> and <off> commands support port abbreviations. These are currently configured as follows:" <</>> tabulate mappingDesc

    mappingDesc =
      M.assocs ps & map \(k, (i, pns)) ->
        ( text k,
          text i <> PP.text "/" <> renderPs pns
        )

    renderPs (p :| []) = text p
    renderPs pns = PP.encloseSep PP.lparen PP.rparen PP.comma (toList $ text <$> pns)

    text = PP.text . T.unpack . coerce

    postProcess :: RawCommand -> IO Command
    postProcess (RawStatus Nothing Nothing) = pure $ Status SAll
    postProcess (RawStatus (Just i) Nothing) = pure $ Status (SInterface i)
    postProcess (RawStatus Nothing (Just p)) =
      case unabbrev portMapping (coerce p) of
        Nothing -> liftIO . throwIO . NoSuchPortAbbrev . coerce $ p
        Just (i, p' :| []) -> pure $ Status (SFullPort i p')
        Just (i, _) -> pure $ Status (SInterface i)
    postProcess (RawStatus (Just i) (Just p)) = pure $ Status (SFullPort i (coerce p))

    postProcess (RawOn Nothing p) =
      case unabbrev portMapping (coerce p) of
        Nothing -> liftIO . throwIO . NoSuchPortAbbrev . coerce $ p
        Just (i, p') -> pure $ On i p'
    postProcess (RawOn (Just i) p) = pure $ On i (coerce p :| [])

    postProcess (RawOff Nothing p) =
      case unabbrev portMapping (coerce p) of
        Nothing -> liftIO . throwIO . NoSuchPortAbbrev . coerce $ p
        Just (i, p') -> pure $ Off i p'
    postProcess (RawOff (Just i) p) = pure $ Off i (coerce p :| [])
