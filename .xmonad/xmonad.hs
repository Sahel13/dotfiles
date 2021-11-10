------------------------------------------------------------------
-- Imports
------------------------------------------------------------------
import XMonad
import System.Exit (exitSuccess)

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops

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

-- Colors for the bar
primaryColor :: String
primaryColor = "#f1600e"

secondaryColor :: String
secondaryColor = "#f4803e"

------------------------------------------------------------------
-- Main
------------------------------------------------------------------
main = xmonad =<< statusBar "xmobar" myPP toggleStrutsKey myConfig

-- Configuration
myConfig = defaultConfig
    -- General settings
    { modMask = mod4Mask
    , terminal = myTerminal
    , focusFollowsMouse = False
    , borderWidth = 1
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#ffd9c9"
    , workspaces = myWorkspaces
    -- Hooks
    , startupHook = myStartupHook
    , layoutHook = myLayoutHook
    , manageHook = myManageHook
    , handleEventHook = fullscreenEventHook
    } `additionalKeysP` myKeys

------------------------------------------------------------------
-- Xmobar
------------------------------------------------------------------
myPP = xmobarPP
    { ppCurrent = xmobarColor primaryColor "" . wrap "[" "]"
    , ppHidden = xmobarColor secondaryColor ""
    , ppTitle = xmobarColor primaryColor "" . shorten 60
    }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

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
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr -- Set cursor style

------------------------------------------------------------------
-- Layout
------------------------------------------------------------------
myLayoutHook = smartBorders ((spacingRaw False (Border mySpacing 0 mySpacing 0) True (Border 0 mySpacing 0 mySpacing) True $ Tall 1 (3/100) (1/2)) ||| Full)

------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "firefox" --> doShift "web"
    , className =? "Mendeley Desktop" --> doShift "main"
    , className =? "zoom" --> doShift "misc"
    , className =? "zoom" --> doFloat
    , className =? "Signal" --> doShift "chat"
    , className =? "TelegramDesktop" --> doShift "chat"
    , className =? "discord" --> doShift "chat"
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
    manageTerm = customFloating $ W.RationalRect (1/4) (1/4) (1/2) (1/2)

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
    , ("M-M1-f w", spawn "firefox -P 'Work'")
    , ("M-M1-f p", spawn "firefox -P 'Personal'")
    , ("M-M1-s", spawn "signal-desktop")
    , ("M-M1-t", spawn "telegram-desktop")
    , ("M-M1-d", spawn "discord")
    , ("M-M1-m", spawn "mendeleydesktop")
    , ("M-M1-z", spawn "zoom")
    , ("M-M1-c", spawn "firefox -P 'Personal' 'https://calendar.protonmail.com/u/0/'")

    -- Email
    , ("M-M1-p 1", spawn "firefox -P 'Personal' 'https://mail.protonmail.com/u/0/inbox'")
    , ("M-M1-p 2", spawn "firefox -P 'Personal' 'https://mail.protonmail.com/u/1/inbox'")
    , ("M-M1-g w", spawn "firefox -P 'Work' 'https://mail.google.com/mail/u/0/#inbox'")
    , ("M-M1-g p", spawn "firefox -P 'Personal' 'https://mail.google.com/mail/u/0/#inbox'")

    -- Function keys
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ]
