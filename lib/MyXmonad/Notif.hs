{-# OPTIONS_GHC -Wno-orphans #-}

module MyXmonad.Notif where

import Control.Monad
import Libnotify
import Libnotify qualified as LN
import XMonad
import XMonad.Util.ExtensibleState qualified as XS

class (MonadIO m) => HasNotifPersistance m where
  getNID :: m (Maybe Notification)
  putNID :: Notification -> m ()

  notif :: Mod Notification -> m ()
  notif n = do
    s :: Maybe Notification <- getNID
    liftIO $ print s
    let msg = n <> maybe mempty reuse s
    putNID =<< liftIO (LN.display msg)

instance HasNotifPersistance IO where
  getNID = return Nothing
  putNID _ = return ()

instance HasNotifPersistance X where
  getNID = XS.get
  putNID = XS.put . Just

instance ExtensionClass (Maybe Notification) where
  initialValue = Nothing

notif' :: (MonadIO m) => String -> m ()
notif' s =
  void $
    liftIO $
      LN.display $
        summary "XMonad notification" <> LN.body s
