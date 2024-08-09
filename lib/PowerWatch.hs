module PowerWatch (watch) where

import DBus
import DBus.Client
import DBus.Socket (open)
import DBus.TH (registerForPropertiesChanged)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Word
import Libnotify (Urgency (..), body, display, display_, summary, urgency)

watch :: IO ()
watch = do
  client <- connectSystem
  -- TODO: poll org.freedesktop.UPower for devices with EnumerateDevices
  let batRule =
        matchAny
          { matchPath = Just "/org/freedesktop/UPower/devices/battery_BAT0"
          }
  _ <- registerForPropertiesChanged client batRule $ \sig _ var ss -> do
    let pass =
          maybe
            (return ())
            ( \(m, u) ->
                display_ $
                  summary "Battery State"
                    <> body ("warning changed to " ++ m)
                    <> urgency u
            )
    pass $ formatState <$> M.lookup "State" var
    pass $ formatWarning <$> M.lookup "WarningLevel" var
  -- TODO: this sucks
  return ()

formatState :: Variant -> (String, Urgency)
formatState = doFormatState . fromMaybe 0 . fromVariant
  where
    doFormatState :: Word32 -> (String, Urgency)
    doFormatState 0 = ("Unknown", Low)
    doFormatState 1 = ("Charging", Low)
    doFormatState 2 = ("Discharging", Normal)
    doFormatState 3 = ("Empty", Critical)
    doFormatState 4 = ("Fully charged", Normal)
    doFormatState 5 = ("Pending charge", Low)
    doFormatState 6 = ("Pending discharge", Low)
    doFormatState _ = ("Invalid value", Low)

formatWarning :: Variant -> (String, Urgency)
formatWarning = doFormatWarning . fromMaybe 0 . fromVariant
  where
    doFormatWarning :: Word32 -> (String, Urgency)
    doFormatWarning 0 = ("Unknown", Low)
    doFormatWarning 1 = ("None", Low)
    doFormatWarning 2 = ("Discharging", Normal)
    doFormatWarning 3 = ("Low", Normal)
    doFormatWarning 4 = ("Critical", Critical)
    doFormatWarning 5 = ("Action", Critical)
    doFormatWarning _ = ("Invalid value", Low)
