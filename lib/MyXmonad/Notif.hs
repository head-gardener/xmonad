{-# OPTIONS_GHC -Wno-orphans #-}

module MyXmonad.Notif where

import Control.Monad
import Libnotify
import Libnotify qualified as LN
import XMonad
import XMonad.Util.ExtensibleState qualified as XS

instance ExtensionClass (Maybe Notification) where
  initialValue = Nothing

notif :: String -> X ()
notif = void . notif'

notif' :: (MonadIO m) => String -> m Notification
notif' s =
  liftIO $
    LN.display $
      summary "XMonad notification" <> LN.body s

persistentNotif :: Mod Notification -> X ()
persistentNotif n = do
  s :: Maybe Notification <- XS.get
  let msg = n <> maybe mempty reuse s
  XS.put . Just =<< liftIO (LN.display msg)
