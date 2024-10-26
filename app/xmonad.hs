{-# LANGUAGE LambdaCase #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}

import Control.Applicative ((<|>))
import Control.Arrow ((&&&))
import Control.Monad (unless)
import Data.Functor ((<&>))
import Data.List (intercalate)
import MyXmonad.CPanel qualified as CP
import MyXmonad.Notif
import PowerWatch qualified as PW
import System.Directory (doesFileExist)
import System.Environment (getEnv)
import XMonad
import XMonad.Actions.Commands
import XMonad.Actions.CycleRecentWS (toggleRecentNonEmptyWS)
import XMonad.Actions.DwmPromote (dwmpromote)
import XMonad.Actions.FindEmptyWorkspace (tagToEmptyWorkspace, viewEmptyWorkspace)
import XMonad.Actions.GridSelect
import XMonad.Actions.Minimize
import XMonad.Actions.Search
import XMonad.Config.Desktop
import XMonad.Hooks.EastGate
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Layout.BoringWindows qualified as BW
import XMonad.Layout.Minimize
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.Spacing
import XMonad.Prompt
import XMonad.Prompt.Ssh (sshPrompt)
import XMonad.Util.EZConfig
import XMonad.Util.Loggers
import XMonad.Util.Run

main :: IO ()
main = do
  bats <- PW.watch
  unless (null bats) $ putStrLn $ "Watching batteries: " ++ intercalate ", " bats
  xmonad
    . ewmh
    . ewmhFullscreen
    . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
    . withMetrics def
    $ myConfig

myConfig =
  desktopConfig
    { terminal = term,
      modMask = mod4Mask,
      manageHook = myManage,
      workspaces = myWorkspaces,
      layoutHook =
        spacingWithEdge 10
          . minimize
          . BW.boringWindows
          $ layout,
      normalBorderColor = "#121212",
      focusedBorderColor = "#1e1e1e",
      borderWidth = 1
    }
    `additionalKeysP` toKeys myKeys
  where
    layout = tiled ||| noBorders Full
      where
        tiled = Tall nmaster delta ratio
        nmaster = 1
        ratio = 13 / 21
        delta = 3 / 100

myWorkspaces :: [String]
myWorkspaces = ["work", "web", "msg", "4", "5", "6", "mail", "8", "9"]

myManage =
  composeAll
    [ className =? "Xmessage" --> doFloat,
      className =? "Conky" --> doIgnore,
      className =? "thunderbird" --> doShift "mail",
      className =? "TelegramDesktop" --> doShift "msg",
      className =? "easyeffects" --> doShift "6",
      manageDocks
    ]

toKeys :: [(String, String, X ())] -> [(String, X ())]
toKeys = fmap (\(a, _, b) -> (a, b))

toCommands :: [(String, String, X ())] -> [(String, X ())]
toCommands = fmap (\(a, b, c) -> (a ++ ": " ++ b, c))

restartFromDev :: X ()
restartFromDev = do
  path <-
    liftIO
      ( getEnv "RELOAD_PATH"
          <|> fmap (<> "/xmonad/result/bin/xmonad") (getEnv "HOME")
          <|> return ""
      )
  exists <- liftIO $ doesFileExist path
  if exists
    then do
      notif' "Restarting..."
      restart path True
    else notif' "Can't restart. Check RELOAD_PATH."

myKeys :: [(String, String, X ())]
myKeys =
  [ ("C-S-<Print>", "screenshot selection to file", spawn "sleep 0.2; scrot -s ~/Screenshots/%Y-%m-%d-%T-screenshot.png"),
    ("S-<Print>", "screenshot selection", spawn "sleep 0.2; scrot -s - | xclip -selection clipboard -t image/png -i"),
    ("C-<Print>", "screenshot to file", spawn "scrot ~/Screenshots/%Y-%m-%d-%T-screenshot.png"),
    ("<Print>", "screenshot", spawn "scrot - | xclip -selection clipboard -t image/png -i"),
    ("M-r", "restart from dev dir", restartFromDev),
    ("M-n", "edit a note", spawn (term ++ " fish -c \"note --select\"")),
    ("M-i", "xprop", spawn "xprop | dmenu"),
    ("<XF86AudioRaiseVolume>", "vol up", spawn "cpanel volup"),
    ("<XF86AudioLowerVolume>", "vol down", spawn "cpanel voldown"),
    ("<XF86AudioMute>", "vol mute", spawn "cpanel volmute"),
    ("<XF86MonBrightnessUp>", "backlight up", CP.blup def),
    ("<XF86MonBrightnessDown>", "backlight down", CP.bldown def),
    ("M-C-<Return>", "spawn no-tmux shell", spawn (term ++ " " ++ shell)),
    ("M-c", "run command", myCommands >>= runCommand),
    ("M-a", "toggle non-empty ws", toggleRecentNonEmptyWS),
    ("M-m", "dwm promote", dwmpromote),
    ("M-e", "find empty", viewEmptyWorkspace),
    ("M-S-e", "tag to empty", tagToEmptyWorkspace),
    ("M-f", "fold", withFocused minimizeWindow),
    ( "M-C-f",
      "unfold",
      withMinimized
        ( \case
            [] -> return ()
            [w] -> maximizeWindowAndFocus w
            ws -> do
              let f :: (Functor m) => (m a, b) -> m (a, b)
                  f (ma, b) = ma <&> (,b)
              ws' <- mapM (f . (getName &&& id)) ws
              w <- gridselect myGSConfig ws'
              maybe (return ()) maximizeWindowAndFocus w
        )
    ),
    ( "M-s",
      "search",
      do
        eng <- gridselect myGSConfig searchEngines
        maybe (return ()) (promptSearch myXPConfig) eng
    ),
    ("M-g", "grid select goto", goToSelected myGSConfig),
    ( "M-C-p",
      "colorpicker",
      do
        c <- runProcessWithInput "xcolor" [] ""
        xmessage c
    )
  ]

getName :: Window -> X String
getName win = do
  class' <- runQuery className win
  name' <- runQuery (stringProperty "WM_NAME") win
  return $ case (class', name') of
    ("", "") -> "undefined"
    (c, "") -> c
    (_, n) -> n

searchEngines :: [(String, SearchEngine)]
searchEngines =
  [ ("duck", duckduckgo),
    ("github", github),
    ( "home-manager",
      searchEngine
        "home-manager"
        "https://mipmip.github.io/home-manager-option-search/?query="
    ),
    ( "nixos-packages",
      searchEngine
        "nixos packages"
        "https://search.nixos.org/packages?channel=24.05&from=0&size=50&sort=relevance&type=packages&query="
    ),
    ( "nixos-options",
      searchEngine
        "nixos options"
        "https://search.nixos.org/options?channel=24.05&size=50&sort=relevance&type=packages&query="
    ),
    ("hackage", hackage),
    ("hoogle", hoogle),
    ("noogle", searchEngine "noogle" "https://noogle.dev/q?term="),
    ("phind", searchEngine "phind" "https://www.phind.com/search?q="),
    ("vocabulary", vocabulary)
  ]

keyRef :: X ()
keyRef = runCommand $ toCommands myKeys

myGSConfig :: GSConfig a
myGSConfig =
  (buildDefaultGSConfig colorizer)
    { gs_navigate = navNSearch,
      gs_cellwidth = 130,
      gs_cellheight = 80,
      gs_font = "xft:LilexNerdFont-medium-9",
      gs_bordercolor = cs_white defaultCS
    }
  where
    colorizer _ True = return (cs_darkgrey defaultCS, cs_white defaultCS)
    colorizer _ False = return (cs_black defaultCS, cs_blue defaultCS)

myCommands :: X [(String, X ())]
myCommands = (++ other) <$> defaultCommands
  where
    other =
      [ ("key-ref", keyRef),
        ("ssh-prompt", sshPrompt myXPConfig)
      ]

myXPConfig :: XPConfig
myXPConfig =
  def
    { bgColor = cs_black defaultCS,
      fgColor = cs_white defaultCS,
      bgHLight = cs_blue defaultCS,
      fgHLight = cs_darkgrey defaultCS,
      borderColor = cs_grey defaultCS,
      font = "xft:LilexNerdFont-medium-9",
      promptBorderWidth = 0,
      maxComplRows = Just 1
    }

myXmobarPP :: PP
myXmobarPP =
  def
    { ppSep = magenta " • ",
      ppTitleSanitize = xmobarStrip,
      ppCurrent = wrap " " "" . xmobarBorder "Top" "#8be9fd" 2,
      ppHidden = white . wrap " " "",
      ppHiddenNoWindows = lowWhite . wrap " " "",
      ppUrgent = red . wrap (yellow "!") (yellow "!"),
      ppOrder = \[ws, l, _, wins] -> [ws, l, wins],
      ppExtras = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused = wrap (white "[") (white "]") . magenta . ppWindow
    formatUnfocused = wrap (white "[") (white "]") . blue . ppWindow

    -- \| Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 20

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta = xmobarColor (cs_pink defaultCS) ""
    blue = xmobarColor (cs_blue defaultCS) ""
    white = xmobarColor (cs_white defaultCS) ""
    yellow = xmobarColor (cs_yellow defaultCS) ""
    red = xmobarColor (cs_red defaultCS) ""
    lowWhite = xmobarColor (cs_darkgrey defaultCS) ""

term, shell :: String
term = "kitty"
shell = "fish"

data ColorScheme = ColorScheme
  { cs_white,
    cs_black,
    cs_darkgrey,
    cs_grey,
    cs_red,
    cs_pink,
    cs_blue,
    cs_magenta,
    cs_yellow ::
      String
  }

defaultCS :: ColorScheme
defaultCS =
  ColorScheme
    { cs_white = "#b3afaf",
      cs_black = "#1c1c1c",
      cs_darkgrey = "#3c3c3c",
      cs_grey = "#9a9a9a",
      cs_red = "#905050",
      cs_pink = "#d0a4a4",
      cs_blue = "#87afaf",
      cs_magenta = "#f1fa8c",
      cs_yellow = "#ffa500"
    }
