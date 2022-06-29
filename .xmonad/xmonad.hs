-- Import Modules

-- Base
import XMonad
import Data.Monoid
import System.Exit

-- Utils
import XMonad.Util.Run
import XMonad.Util.SpawnOnce
import XMonad.Util.Loggers
import XMonad.Util.EZConfig

-- Hooks
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.StatusBar
import XMonad.Hooks.StatusBar.PP
import XMonad.Hooks.EwmhDesktops

-- Layout modifiers
import XMonad.Layout.Spacing

-- Actions
import XMonad.Actions.SpawnOn
import XMonad.Actions.CopyWindow (kill1)
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.Promote

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- Variables
myTerminal = "alacritty"
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False -- I don't use a mouse anymore
myClickJustFocuses :: Bool
myClickJustFocuses = False
myBorderWidth = 2
myModMask = mod4Mask
myNormalBorderColor = "#dddddd"
myFocusedBorderColor = "#FF0000"

myWorkspaces = [" term ", " www ", " mpv ", " doc ", " dev ", " chat ", "mail", "alsa", "sys"]

-- Key bindings
myKeys = \c -> mkKeymap c $
    [ -- XMonad session control
      ("M-S-r", spawn "xmonad --recompile && xmonad --restart")
    , ("M-S-q", io (exitWith ExitSuccess))
    , ("M-S-c", kill1)
    , ("M-S-a", killAll)

    -- Window management
    , ("M-h", sendMessage Shrink)
    , ("M-l", sendMessage Expand)
    , ("M-j", windows W.focusDown)
    , ("M-k", windows W.focusUp)
    , ("M-m", windows W.focusMaster)
    , ("M-S-j", windows W.swapDown)
    , ("M-S-k", windows W.swapUp)
    , ("M-n", refresh)
    , ("M-t", withFocused $ windows . W.sink)
    , ("M-<Backspace>", promote)
    , ("M-<Space>", sendMessage NextLayout)
    , ("M-c", kill)

    -- Launch applications
    , ("M-S-<Return>", spawn $ terminal c)
    , ("M-<Return>", spawn "dmenu_run")
    , ("M-b", spawn "qutebrowser --qt-flag disable-seccomp-filter-sandbox")

    -- Switch to workspace
    , ("M-1", (windows $ W.greedyView $ myWorkspaces !! 0))
    , ("M-2", (windows $ W.greedyView $ myWorkspaces !! 1))
    , ("M-3", (windows $ W.greedyView $ myWorkspaces !! 2))
    , ("M-4", (windows $ W.greedyView $ myWorkspaces !! 3))
    , ("M-5", (windows $ W.greedyView $ myWorkspaces !! 4))
    , ("M-6", (windows $ W.greedyView $ myWorkspaces !! 5))
    , ("M-7", (windows $ W.greedyView $ myWorkspaces !! 6))
    , ("M-8", (windows $ W.greedyView $ myWorkspaces !! 7))
    , ("M-9", (windows $ W.greedyView $ myWorkspaces !! 8))

    -- Manually send window to workspace
    , ("M-S-1", (windows $ W.shift $ myWorkspaces !! 0))
    , ("M-S-2", (windows $ W.shift $ myWorkspaces !! 1))
    , ("M-S-3", (windows $ W.shift $ myWorkspaces !! 2))
    , ("M-S-4", (windows $ W.shift $ myWorkspaces !! 3))
    , ("M-S-5", (windows $ W.shift $ myWorkspaces !! 4))
    , ("M-S-6", (windows $ W.shift $ myWorkspaces !! 5))
    , ("M-S-7", (windows $ W.shift $ myWorkspaces !! 6))
    , ("M-S-8", (windows $ W.shift $ myWorkspaces !! 7))]

-- Mouse bindings: default actions bound to mouse events
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))
    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))
    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))]

-- Layouts:
myLayout = avoidStruts(tiled ||| Mirror tiled ||| Full)
  where
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

-- Window rules:
myManageHook = composeAll
    [ className =? "qutebrowser"   --> doShift ( myWorkspaces !! 1 )
    , className =? "chromium"   --> doShift ( myWorkspaces !! 1 )
    -- , className =? "Gimp"           --> doFloat
    -- , resource  =? "desktop_window" --> doIgnore
    -- , resource  =? "kdesktop"       --> doIgnore 
    ]

myEventHook = mempty

myLogHook = return ()

myStartupHook = do
  spawnOnce "feh --bg-scale ~/Wallpapers/main.png"
  spawnOnce "picom &"
  spawnOnce "xmodmap ~/.xmodmap"
  spawnOnce "unclutter -idle 0.01 -root"
  spawnOnce "xcape -e 'Super_L=Escape'"

  -- Launch applications on startup
  spawnOn (myWorkspaces !! 0) "alacritty"
  spawnOn (myWorkspaces !! 1) "qutebrowser --qt-flag disable-seccomp-filter-sandbox"
  spawnOn (myWorkspaces !! 2) "emacs -f elfeed"
  spawnOn (myWorkspaces !! 3) "emacs"
  spawnOn (myWorkspaces !! 4) "emacs"
  spawnOn (myWorkspaces !! 5) "emacs -f chat/connect-ement"
  spawnOn (myWorkspaces !! 6) "emacs -f notmuch"
  spawnOn (myWorkspaces !! 7) "alacritty -e alsamixer"
  spawnOn (myWorkspaces !! 8) "alacritty -e btop"

main :: IO ()
main = xmonad
     . ewmhFullscreen
     . ewmh
     . withEasySB (statusBarProp "xmobar" (pure myXmobarPP)) toggleStrutsKey
     $ myConfig
  where
    toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
    toggleStrutsKey XConfig{ modMask = m } = (m, xK_w)

myXmobarPP :: PP
myXmobarPP = def
    { ppSep             = yellow " * "
    , ppTitleSanitize   = xmobarStrip
    , ppCurrent         = wrap " " "" . xmobarBorder "Bottom" "#bd93f9" 2
    , ppHidden          = white . wrap " " ""
    , ppHiddenNoWindows = lowWhite . wrap " " ""
    , ppUrgent          = red . wrap (yellow "!") (yellow "!")
    , ppOrder           = \[ws, l, _, wins] -> [ws, l, wins]
    , ppExtras          = [logTitles formatFocused formatUnfocused]
    }
  where
    formatFocused   = wrap (white    "[") (white    "]") . magenta . ppWindow
    formatUnfocused = wrap (lowWhite "[") (lowWhite "]") . blue    . ppWindow

    -- | Windows should have *some* title, which should not not exceed a
    -- sane length.
    ppWindow :: String -> String
    ppWindow = xmobarRaw . (\w -> if null w then "untitled" else w) . shorten 30

    blue, lowWhite, magenta, red, white, yellow :: String -> String
    magenta  = xmobarColor "#ff79c6" ""
    blue     = xmobarColor "#bd93f9" ""
    white    = xmobarColor "#f8f8f2" ""
    yellow   = xmobarColor "#f1fa8c" ""
    red      = xmobarColor "#ff5555" ""
    lowWhite = xmobarColor "#bbbbbb" ""

myConfig = def {
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,
        keys               = myKeys,
        mouseBindings      = myMouseBindings,
        layoutHook         = myLayout,
        manageHook = manageSpawn <+> manageHook def,
        handleEventHook    = myEventHook,

        startupHook        = myStartupHook,
        logHook            = myLogHook
    }
