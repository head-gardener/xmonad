module MyXmonad.Notif where

import Control.Monad
import XMonad
import Libnotify
import Libnotify qualified as LN

notif :: String -> X ()
notif s =
  liftIO $
    void $
      LN.display $
        summary "XMonad notification" <> LN.body s
