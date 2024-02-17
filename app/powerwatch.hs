module Main where

import Control.Concurrent (threadDelay)
import Control.Monad (forever)
import DBus
import DBus.Client
import DBus.Socket (open)
import DBus.TH (registerForPropertiesChanged)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Libnotify (body, display, display_, summary)
import Data.Word

main :: IO ()
main = do
  client <- connectSystem
  -- TODO: notification style (low urg, crit, etc.)
  -- TODO: poll org.freedesktop.UPower for devices with EnumerateDevices
  let batRule =
        matchAny
          { matchPath = Just "/org/freedesktop/UPower/devices/battery_BAT0"
          }
  _ <- registerForPropertiesChanged client batRule $ \sig _ var ss -> do
    let s = M.lookup "State" var
    let w = M.lookup "WarningLevel" var
    maybe
      (return ())
      ( \v ->
          display_ $ summary "Battery State" <> body ("warning changed to " ++ formatWarning v)
      )
      w
    maybe
      (return ())
      ( \v ->
          display_ $ summary "Battery State" <> body ("state changed to " ++ formatState v)
      )
      s
  -- TODO: this sucks
  forever $ threadDelay 100000000000
  where
    formatState :: Variant -> String
    formatState = doFormatState . fromMaybe 0 . fromVariant

    doFormatState :: Word32 -> String
    doFormatState 0 = "Unknown"
    doFormatState 1 = "Charging"
    doFormatState 2 = "Discharging"
    doFormatState 3 = "Empty"
    doFormatState 4 = "Fully charged"
    doFormatState 5 = "Pending charge"
    doFormatState 6 = "Pending discharge"
    doFormatState _ = "Invalid value"

    formatWarning :: Variant -> String
    formatWarning = doFormatWarning . fromMaybe 0 . fromVariant

    doFormatWarning :: Word32 -> String
    doFormatWarning 0 = "Unknown"
    doFormatWarning 1 = "None"
    doFormatWarning 2 = "Discharging"
    doFormatWarning 3 = "Low"
    doFormatWarning 4 = "Critical"
    doFormatWarning 5 = "Action"
    doFormatWarning _ = "Invalid value"
