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

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (smartBorders)

-- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.Cursor

-- Keybindings
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Actions.Submap

-- Scratchpads
import XMonad.Util.NamedScratchpad
import qualified XMonad.StackSet as W

-- Multi-monitor support
import XMonad.Actions.CycleWS (nextScreen, prevScreen)

------------------------------------------------------------------
-- Variables
------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["main", "work", "web", "code", "chat", "misc"]

myTerminal :: String
myTerminal = "alacritty"

mySpacing :: Integer
mySpacing = 5

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
-- Autostart
------------------------------------------------------------------
myStartupHook :: X ()
myStartupHook = do
    spawnOnce "~/.fehbg &" -- Wallpaper
    spawnOnce "picom -b &" -- Compositor
    spawnOnce "/usr/lib/polkit-gnome/polkit-gnome-authentication-agent-1 &" -- Authentication agent
    spawnOnce "/usr/bin/dunst &" -- Notification server
    spawnOnce "xfce4-power-manager &" -- Power manager
    spawnOnce "unclutter &" -- Hide the cursor
    spawnOnce "trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 5 --transparent true --tint 0x1d1d1d --height 18 &"
    spawnOnce "nm-applet &"
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr -- Set cursor style

------------------------------------------------------------------
-- Layout
------------------------------------------------------------------
myLayoutHook = smartBorders ((spacingRaw False (Border mySpacing 0 mySpacing 0) True (Border 0 mySpacing 0 mySpacing) True $ tiled) ||| Full)
  where
    tiled   = Tall nmaster delta ratio
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
    , className =? "TelegramDesktop" --> doShift "chat"
    , className =? "discord" --> doShift "chat"
    , className =? "Anki" --> doShift "misc"
    , className =? "vlc" --> doShift "misc"
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
    , ("M-S-b", spawn "blueman-manager")
        -- Scratchpads
    , ("M-C-<Return>", namedScratchpadAction myScratchpads "terminal")
    , ("M-S-n", namedScratchpadAction myScratchpads "network")
    , ("M-S-h", namedScratchpadAction myScratchpads "htop")

    -- Multi-monitor navigation
    , ("M-.", nextScreen)
    , ("M-,", prevScreen)

    -- Applications
    , ("M-S-<Return>", spawn (myTerminal ++ " -e ranger"))
    , ("M1-S-<Return>", spawn "thunar")
    , ("M-M1-z", spawn "zotero")
    , ("M-M1-f", spawn "firefox")
    , ("M-M1-v", spawn "vivaldi-stable")
    , ("M-M1-s", spawn "signal-desktop")
    , ("M-M1-t", spawn "telegram-desktop")
    , ("M-M1-d", spawn "discord")

    -- Email
    , ("M-M1-p 1", spawn "firefox 'https://mail.proton.me/u/0/inbox'")
    , ("M-M1-p 2", spawn "firefox 'https://mail.proton.me/u/1/inbox'")
    , ("M-M1-g p", spawn "firefox 'https://mail.google.com/mail/u/0/#inbox'")
    , ("M-M1-g w", spawn "firefox 'https://mail.google.com/mail/u/1/#inbox'")

    -- Function keys
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ]
