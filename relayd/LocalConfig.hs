module LocalConfig (getCfg) where

import Config

getCfg :: IO Config
getCfg = makeConfig (Nothing, Nothing)
  [ InterfaceConfig { name = "rack"
                    , devicePath = "/dev/spidev0.0"
                    , portConfig = PortConfig []
                    }
  , InterfaceConfig { name = "closet"
                    , devicePath = "/dev/spidev0.1"
                    , portConfig = PortConfig [ (1, "chlorine-aux")
                                              , (5, "chlorine")
                                              ]
                    }
  ]
