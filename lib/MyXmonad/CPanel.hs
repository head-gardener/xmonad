module MyXmonad.CPanel (volmute, blup, bldown, volmute', doBacklight') where

import Libnotify
import Libnotify qualified as LN
import MyXmonad.Notif
import XMonad
import XMonad.Util.Run (runProcessWithInput)
import Data.List.Split (splitOn)

volmute' :: IO String
volmute' = do
  runProcessWithInput "pw-volume" ["mute", "toggle"] ""

volmute :: X ()
volmute = do
  s <- runProcessWithInput "pw-volume" ["mute", "toggle"] ""
  pmnotif

doNotify :: Mod Notification -> X ()
doNotify n =
  persistentNotif $
    LN.urgency LN.Low <> LN.timeout (LN.Custom 1500) <> n

doBacklight' :: Char -> IO String
doBacklight' op = do
  formattter <$> runProcessWithInput
    "brightnessctl"
    ["set", "5%" ++ [op], "-m"]
    ""
  where
    -- "intel_backlight,backlight,7500,100%,7500" -> "100%"
    formattter :: String -> String
    formattter = (!! 3) . splitOn ","

doBacklight :: Char -> X ()
doBacklight op = do
  br <- liftIO $ doBacklight' op
  -- br <- runProcessWithInput "brightnessctl"
  -- ["set", "-e", "'5%" ++ op ++"'", "-m"] ""
  doNotify $ LN.summary "Backlight" <> LN.body ("set to " ++ br)

blup, bldown :: X ()
blup = doBacklight '+'
bldown = doBacklight '-'

pmnotif :: X ()
pmnotif = do
  doNotify $ LN.summary "Volume" <> LN.body "muting"

-- pwnotif() {
--   tt=$(pw-volume status\
--     | jq -r\
--     'if .percentage then "\(.percentage)%" else .tooltip end')
--   notif "Volume" "set to $tt"
-- }

-- blnotif() {
--   br=$(brightnessctl set -e "5%$1" -m | cut -d',' -f4)
--   notif "Backlight" "set to $br"
-- }

-- case "$1" in
--   volup)
--     pw-volume change '+5%'
--     pwnotif
--     ;;

--   voldown)
--     pw-volume change '-5%'
--     pwnotif
--     ;;

--   volmute)
--     pw-volume mute toggle
--     pwnotif
--     ;;

--   blup)
--     blnotif '+'
--     ;;

--   bldown)
--     blnotif '-'
--     ;;

--   *)
--     notif "Error" "Invalid command \"$1\" sent to cpanel"
--     ;;
-- esac
