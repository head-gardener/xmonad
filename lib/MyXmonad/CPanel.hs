module MyXmonad.CPanel (CPConfig, def, blup, bldown, volmute, volup, voldown) where

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
import Text.Printf (printf)
import XMonad (Default (..))
import XMonad.Util.Run (runProcessWithInput)

-- remove once we get 0.6 transformers
hoistMaybe :: (Applicative m) => Maybe b -> MaybeT m b
hoistMaybe = MaybeT . pure

data PWStatus = PWStatus
  { tooltip :: String
  , percentage :: Maybe Int
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

volmute, volup, voldown, blup, bldown :: (HasNotifPersistance m) => CPConfig -> m ()
blup = doBacklight '+'
bldown = doBacklight '-'
volmute = doPipewire ["mute", "toggle"]
volup = doPipewire ["change", "+5%"]
voldown = doPipewire ["change", "-5%"]

doBacklight :: (HasNotifPersistance m) => BacklightOp -> CPConfig -> m ()
doBacklight op c = do
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

doPipewire :: (HasNotifPersistance m) => [String] -> CPConfig -> m ()
doPipewire args c = do
  -- TODO: do this with exception or sum
  void $ runProcessWithInput "pw-volume" args ""
  tt <- fmap (fromMaybe "Internall Error") $ runMaybeT $ do
    (_, mhout, _, _) <-
      liftIO
        $ createProcess (shell "pw-volume status"){std_out = CreatePipe}
    hout <- hoistMaybe mhout
    stat' <- liftIO $ BL.hGetContents hout
    stat'' <- hoistMaybe $ decode stat'
    return $ maybe (tooltip stat'') (printf "set to %i%%") $ percentage stat''
  notif $ notifStyle c <> LN.summary "Volume" <> LN.body tt
