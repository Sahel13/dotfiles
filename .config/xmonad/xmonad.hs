------------------------------------------------------------------
-- Imports
------------------------------------------------------------------
import XMonad
import System.Exit (exitSuccess)

import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Util.Loggers (logClassnames)
import XMonad.Hooks.ManageHelpers (isDialog)

-- Layouts
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.ResizableTile
import XMonad.Layout.TwoPane
import XMonad.Layout.Magnifier
import XMonad.Layout.Renamed
import qualified XMonad.Layout.ToggleLayouts as TL

-- Keybindings
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.Submap
import XMonad.Actions.CycleWS

-- Scratchpads
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

------------------------------------------------------------------
-- Variables
------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["main", "latex", "web", "code", "chat", "misc"]

myTerminal :: String
myTerminal = "alacritty"

------------------------------------------------------------------
-- Main
------------------------------------------------------------------
main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) defToggleStrutsKey
     $ myConfig

-- Configuration
myConfig = def
    -- General settings
    { modMask = mod4Mask
    , terminal = myTerminal
    , focusFollowsMouse = False
    , borderWidth = 2
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#076678"
    , workspaces = myWorkspaces
    -- Hooks
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    } `additionalKeysP` myKeys

------------------------------------------------------------------
-- Xmobar
------------------------------------------------------------------
myXmobarPP :: PP
myXmobarPP = def
    { ppSep     = red " • "
    , ppCurrent = wrap "" "" . xmobarBorder "Top" "#ff0000" 2
    , ppVisible = white
    , ppHidden  = offWhite
    , ppUrgent  = red . wrap (yellow "!") (yellow "!")
    , ppOrder   = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras  = [logClassnames formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white "[") (white "]") . red . ppWindow
    formatUnfocused = wrap (offWhite "[") (offWhite "]") . brown . ppWindow

    ppWindow :: String -> String
    ppWindow s | s == "Navigator" = xmobarRaw "Firefox"
               | otherwise = xmobarRaw $ shorten 25 s

    offWhite, brown, red, white, yellow :: String -> String
    brown  = xmobarColor "#a52a2a" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    offWhite = xmobarColor "#bbbbbb" ""

------------------------------------------------------------------
-- StartupHook
------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do setWMName "LG3D" -- Needed for PyCharm

------------------------------------------------------------------
-- Layout
------------------------------------------------------------------
myLayoutHook = smartBorders $ TL.toggleLayouts twopane (tiled ||| Full)
  where
    tiled = renamed [Replace "Tall"] $ magnifierczOff' 1.3
                                       $ ResizableTall 1 delta ratio []
    twopane = TwoPane delta ratio
    delta = 3/100
    ratio = 1/2

------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "firefox" --> doShift "web"
    , className =? "Signal" --> doShift "chat"
    , className =? "vlc" --> doShift "misc"
    , className =? "zoom" --> doShift "main"
    , className =? "zoom" --> doFloat
    , isDialog --> doFloat
    , namedScratchpadManageHook myScratchpads
    ]

------------------------------------------------------------------
-- Scratchpads
------------------------------------------------------------------
myScratchpads =
    [ NS "terminal" spawnTerm findTerm manageTerm
    , NS "network" spawnNetwork findNetwork manageNetwork 
    , NS "htop" spawnHtop findHtop manageHtop 
    ]
  where
    spawnTerm = myTerminal ++ " -t terminal"
    findTerm = title =? "terminal"
    manageTerm = customFloating $ W.RationalRect (1/5) (1/5) (3/5) (3/5)

    spawnNetwork = myTerminal ++ " -t network -e nmtui"
    findNetwork = title =? "network"
    manageNetwork = customFloating $ W.RationalRect (1/4) (1/5) (2/4) (3/5)

    spawnHtop = myTerminal ++ " -t htop -e htop"
    findHtop = title =? "htop"
    manageHtop = customFloating $ W.RationalRect (1/12) (1/6) (10/12) (4/6)

------------------------------------------------------------------
-- Keybindings
------------------------------------------------------------------
myKeys :: [(String, X ())]
myKeys =
    -- Essentials
    [ ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q", io exitSuccess)
    , ("M-q", kill)
    , ("M-<Return>", spawn myTerminal)

    -- Utilities
    , ("M-d", spawn "dmenu_run")
    , ("<Print>", spawn "flameshot gui")
    , ("M-S-x", spawn "/home/sahel/.local/scripts/screen_lock.sh")

        -- Scratchpads
    , ("M-C-<Return>", namedScratchpadAction myScratchpads "terminal")
    , ("M-S-n", namedScratchpadAction myScratchpads "network")
    , ("M-S-h", namedScratchpadAction myScratchpads "htop")

    -- Multi-monitor navigation
    , ("M-.", nextScreen)
    , ("M-,", prevScreen)

    -- Toggle between current and previous workspaces.
    , ("M-<Tab>", toggleWS' ["NSP"])

    -- Toggle magnification in the Tall layout.
    , ("M-S-m", sendMessage Toggle)

    -- Shrink/expand windows vertically in the Tall layout.
    , ("M-a", sendMessage MirrorShrink)
    , ("M-z", sendMessage MirrorExpand)

    , ("M-S-<Space>", sendMessage TL.ToggleLayout)

    -- Applications
    , ("M-S-<Return>", spawn (myTerminal ++ " -e ranger"))
    , ("M1-S-<Return>", spawn "thunar")
    , ("M-M1-f", spawn "firefox")
    , ("M-M1-s", spawn "signal-desktop")

    -- Function keys
    , ("<XF86MonBrightnessUp>", spawn "/home/sahel/.local/scripts/backlight.sh +")
    , ("<XF86MonBrightnessDown>", spawn "/home/sahel/.local/scripts/backlight.sh -")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ]
