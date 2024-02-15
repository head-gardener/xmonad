module MyXmonad.CPanel (CPConfig, def, blup, bldown) where

import Control.Monad (void)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Trans.Maybe
import Data.Aeson (FromJSON, decode)
import Data.ByteString.Lazy qualified as BL
import Data.List.Split (splitOn)
import Data.Maybe (fromMaybe)
import GHC.Generics (Generic)
import Libnotify
import Libnotify qualified as LN
import MyXmonad.Notif
import System.Process
import XMonad (Default (..))
import XMonad.Util.Run (runProcessWithInput)

-- remove once we get 0.6 transformers
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

data PWStatus = PWStatus
  { tooltip :: String,
    percentage :: Int
  }
  deriving (Generic, Show)

instance FromJSON PWStatus

newtype CPConfig = CPConfig
  { notifStyle :: Mod Notification
  }

instance Default CPConfig where
  def =
    CPConfig
      { notifStyle = LN.urgency LN.Low <> LN.timeout (LN.Custom 1500)
      }

type BacklightOp = Char

doBacklight :: (HasNotifPersistance m) => CPConfig -> BacklightOp -> m ()
doBacklight c op = do
  br <- formatter <$> liftIO command
  notif $ notifStyle c <> LN.summary "Backlight" <> LN.body ("set to " ++ br)
  where
    formatter = (!! 3) . splitOn ","

    command :: IO String
    command =
      runProcessWithInput
        "brightnessctl"
        ["set", "5%" ++ [op], "-m"]
        ""

volmute, volup, voldown, blup, bldown :: (HasNotifPersistance m) => CPConfig -> m ()
blup c = doBacklight c '+'
bldown c = doBacklight c '-'
volmute c = do
  void $ runProcessWithInput "pw-volume" ["mute", "toggle"] ""
  pmnotif c
volup c = do
  void $ runProcessWithInput "pw-volume" ["change", "+1%"] ""
  pmnotif c
voldown c = do
  void $ runProcessWithInput "pw-volume" ["change", "-1%"] ""
  pmnotif c

pmnotif :: (HasNotifPersistance m) => CPConfig -> m ()
pmnotif c = do
  tt <- fmap (fromMaybe "Internall Error") $ runMaybeT $ do
    (_, mstat, _, _) <- liftIO $ createProcess (proc "pw-volume" ["status"])
    stat <- hoistMaybe mstat
    stat' <- liftIO $ BL.hGetContents stat
    -- stat'' <- hoistMaybe $ decode stat'
    return $ show stat' -- tooltip stat''
  pmnotif c
  notif $ notifStyle c <> LN.summary "Volume" <> LN.body tt

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
