------------------------------------------------------------------
-- Imports
------------------------------------------------------------------
import XMonad
import System.Exit (exitSuccess)

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.ManageHelpers

-- Layouts
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.TwoPane
import XMonad.Layout.Magnifier

-- Keybindings
import XMonad.Util.EZConfig (additionalKeysP)
-- import XMonad.Actions.Submap

-- Scratchpads
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

-- Multi-monitor support
import XMonad.Actions.CycleWS (toggleWS', nextScreen, prevScreen)

------------------------------------------------------------------
-- Variables
------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["main", "work", "web", "code", "chat", "misc"]

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
    , borderWidth = 3
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
    { ppCurrent = accentColor . wrap "[" "]"
    , ppHidden = accentColor
    , ppTitle = accentColor . shorten 40
    }
  where
    accentColor = xmobarColor "#32827f" ""

------------------------------------------------------------------
-- StartupHook
------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    setWMName "LG3D" -- Needed for PyCharm

------------------------------------------------------------------
-- Layout
------------------------------------------------------------------
myLayoutHook = smartBorders (tiled ||| Full ||| twopane)
  where
    tiled   = magnifierczOff' 1.3 (Tall nmaster delta ratio)
    twopane = TwoPane delta ratio
    nmaster = 1      -- Default number of windows in the master pane
    delta   = 3/100  -- Percent of the screen to increment by when resizing panes
    ratio   = 1/2    -- Default proportion of the screen occupied by the master pane

------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "Zotero" --> doShift "main"
    , className =? "firefox" --> doShift "web"
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
    , ("M-s", spawn "flameshot gui")
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
