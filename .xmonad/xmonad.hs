------------------------------------------------------------------
-- Imports
------------------------------------------------------------------
import XMonad
import System.Exit (exitSuccess)

-- Hooks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.DynamicLog

-- Layouts
import XMonad.Layout.Spacing
import XMonad.Layout.NoBorders (smartBorders)

-- Utilities
import XMonad.Util.SpawnOnce
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Cursor

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
    } `additionalKeysP` myKeys

------------------------------------------------------------------
-- Xmobar
------------------------------------------------------------------
-- Colors for the bar
myPP = xmobarPP
    { ppCurrent = xmobarColor "#ffa07a" "" . wrap "[" "]"
    , ppHidden = xmobarColor "#ffd9c9" ""
    , ppTitle = xmobarColor "#ffa07a" "" . shorten 60
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
    spawnOnce "/usr/lib/xfce4/notifyd/xfce4-notifyd"-- Notification server
    spawnOnce "xfce4-power-manager &" -- Power manager
    spawnOnce "unclutter &" -- Hide the cursor
    setWMName "LG3D"
    setDefaultCursor xC_left_ptr -- Set cursor style

------------------------------------------------------------------
-- Layout
------------------------------------------------------------------
myLayoutHook = smartBorders ((spacingRaw False (Border 8 0 8 0) True (Border 0 8 0 8) True $ Tall 1 (3/100) (1/2)) ||| Full)

------------------------------------------------------------------
-- Window rules
------------------------------------------------------------------
myManageHook = composeAll
    [ className =? "firefox" --> doShift "web"
    , className =? "Mendeley Desktop" --> doShift "main"
    , className =? "Signal" --> doShift "chat"
    , className =? "TelegramDesktop" --> doShift "chat"
    , className =? "vlc" --> doShift "misc"
    , className =? "Anki" --> doShift "misc"
    ]

------------------------------------------------------------------
-- Variables
------------------------------------------------------------------
myWorkspaces :: [String]
myWorkspaces = ["main", "work", "web", "chat", "misc"]

myTerminal :: String
myTerminal = "alacritty"

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
    , ("M-S-n", spawn (myTerminal ++ " -e nmtui"))
    , ("M-S-h", spawn (myTerminal ++ " -e htop"))
    , ("M-S-b", spawn "blueman-manager")

    -- Applications
    , ("M-S-<Return>", spawn (myTerminal ++ " -e ranger"))
    , ("M1-S-<Return>", spawn "thunar")
    , ("M-M1-f", spawn "firefox")
    , ("M-M1-s", spawn "signal-desktop")
    , ("M-M1-t", spawn "telegram-desktop")
    , ("M-M1-a", spawn "anki")
    , ("M-M1-d", spawn "discord")
    , ("M-M1-m", spawn "mendeleydesktop")
    , ("M-M1-z", spawn "zoom")

    -- Email
    , ("M-M1-p", spawn ("firefox" ++ " https://account.protonmail.com/switch"))
    , ("M-M1-g", spawn ("firefox" ++ " https://mail.google.com/mail/u/0/#inbox"))
    , ("M-M1-l", spawn ("firefox" ++ " http://localhost:8000/"))

    -- Function keys
    , ("<XF86MonBrightnessUp>", spawn "xbacklight -inc 2")
    , ("<XF86MonBrightnessDown>", spawn "xbacklight -dec 2")
    , ("<XF86AudioRaiseVolume>", spawn "amixer set Master 5%+")
    , ("<XF86AudioLowerVolume>", spawn "amixer set Master 5%-")
    , ("<XF86AudioMute>", spawn "amixer set Master toggle")
    ]
