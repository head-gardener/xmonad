module PowerWatch (watch, getBatteries) where

import Control.Applicative ((<|>))
import Control.Exception (try)
import Control.Monad (forM_)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Maybe
import DBus
import DBus.Client
import DBus.Internal.Types
import DBus.TH (registerForPropertiesChanged)
import Data.Either (fromRight)
import Data.List (isPrefixOf)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Word
import Libnotify (Urgency (..), body, display_, summary, urgency)

getBatteries :: Client -> IO (Maybe [ObjectPath])
getBatteries client = do
  reply <-
    call_
      client
      (methodCall "/org/freedesktop/UPower" "org.freedesktop.UPower" "EnumerateDevices")
        { methodCallDestination = Just "org.freedesktop.UPower"
        }
  let devs = fromVariant $ head $ methodReturnBody reply
  return $ filter (isPrefixOf "battery" . last . pathElements) <$> devs

watch :: IO [String]
watch = do
  client <- connectSystem

  bats <- runMaybeT $ do
    bats <- liftIO (try @ClientError (getBatteries client)) >>= hoistMaybe . fromRight Nothing
    let batRules = map (\b -> matchAny {matchPath = Just b}) bats
    liftIO $ forM_ batRules $ \r -> registerForPropertiesChanged client r $ \_ _ var _ -> do
      let msg =
            formatState <$> M.lookup "State" var
              <|> formatWarning <$> M.lookup "WarningLevel" var
      maybe (return ()) notify msg
    return $ fmap (last . pathElements) bats
  return $ fromMaybe [] bats

notify :: (String, Urgency) -> IO ()
notify (m, u) =
  display_ $
    summary "Battery State"
      <> body ("warning changed to " ++ m)
      <> urgency u

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
