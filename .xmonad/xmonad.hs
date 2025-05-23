--
-- xmonad example config file.
--
-- A template showing all available configuration hooks,
-- and how to override the defaults in your own xmonad.hs conf file.
--
-- Normally, you'd only override those defaults you care about.
--

import XMonad
import Control.Monad (filterM)
import Data.Char (isAscii)
import Data.Monoid
import System.Directory (doesFileExist, getHomeDirectory)
import System.Exit

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

import XMonad.Layout.Spacing
import XMonad.Layout.Renamed

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.EwmhDesktops

import XMonad.Util.WorkspaceCompare (getSortByTag)

-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvtc"
--myTerminal      = "mlclient"
--myTerminal      = "mlterm"

-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = False

-- Whether clicking on a window to focus also passes the click to the window
myClickJustFocuses :: Bool
myClickJustFocuses = False

-- Width of the window border in pixels.
--
myBorderWidth   = 1

-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask       = mod4Mask

-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--
myWorkspaces    = ["1A","2A","3A","4A","5A","6A","7A","8A","9A","1B","2B","3B","4B","5B","6B","7B","8B","9B"]

-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#1A1A1A"
myFocusedBorderColor = "#BCBCBC"

------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return  ), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_KP_Enter), spawn $ XMonad.terminal conf)
    , ((modm .|. shiftMask, xK_x       ), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p         ), spawn "dmenu_run -l 10 -m 1")

    -- launch gmrun
    -- , ((modm .|. shiftMask, xK_p     ), spawn "gmrun")

    -- close focused window
    --, ((modm .|. shiftMask, xK_c     ), kill)
    , ((modm,               xK_c     ), kill)

    -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)
    -- PreviousLayout (dirty?)
    , ((modm .|. shiftMask, xK_space ), sendMessage NextLayout >> sendMessage NextLayout >> sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm,               xK_0     ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)
    , ((modm .|. shiftMask, xK_Tab   ), windows W.focusUp  ) -- previous window

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return  ), windows W.swapMaster)
    , ((modm,               xK_KP_Enter), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Toggle the status bar gap
    -- Use this binding with avoidStruts from Hooks.ManageDocks.
    -- See also the statusBar function from Hooks.DynamicLog.
    --
    -- , ((modm              , xK_b     ), sendMessage ToggleStruts)

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_Escape     ), io (exitWith ExitSuccess))

    -- Restart xmonad
    , ((modm              , xK_Escape     ), spawn "xmonad --recompile; xmonad --restart")

    -- Run xmessage with a summary of the default keybindings (useful for beginners)
    , ((modm .|. shiftMask, xK_slash ), spawn ("echo \"" ++ help ++ "\" | xmessage -file -"))

    -- AlsaMixer
    , ((modm,               xK_minus     ), spawn "amixer -q set Master 655-"   )
    , ((modm .|. shiftMask, xK_semicolon ), spawn "amixer -q set Master 655+"   )
    , ((modm .|. shiftMask, xK_m         ), spawn "amixer -q set Master toggle")

    -- XScreenSaver
    --, ((modm .|. shiftMask, xK_l         ), spawn "xscreensaver-command -lock")

    -- scrot
    --, ((modm,               xK_Print     ), spawn "scrot \"a.png\" -e 'mv $f ~/data/pic/scrot/tmp && xclip -selection clipboard -t image/png ~/data/pic/scrot/tmp/a.png'")
    --, ((modm .|. shiftMask, xK_Print     ), spawn "scrot -e 'mv $f ~/data/pic/scrot/'")

    -- suspend
    , ((modm,               xK_Pause     ), spawn "systemctl suspend")
    -- hibernate
    , ((modm .|. shiftMask, xK_Pause     ), spawn "systemctl hibernate")
    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) ([xK_1 .. xK_9] ++ [xK_F1 .. xK_F9])
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_q, xK_w, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $

    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Raise the window to the top of the stack
    , ((modm, button2), (\w -> focus w >> windows W.shiftMaster))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))

    -- you may also bind events to the mouse scroll wheel (button4 and button5)
    ]

------------------------------------------------------------------------
-- Layouts:

-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
myLayout = avoidStruts standardLayout
  where
     standardLayout =   renamed [Replace ":: Tall ::"] tiled
                    ||| renamed [Replace ":: Mirror ::"] mirrorTiled
                    ||| renamed [Replace ":: Max ::"] (spacing 10 Full)
                    ||| renamed [Replace ":: Full ::"] (spacing 0 Full)
     -- default tiling algorithm partitions the screen into two panes
     tiled   = spacing 10 $ Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100
     -- Mirror Tiled
     mirrorTiled  = Mirror . spacing 10 $ Tall nmaster delta (70/100)

------------------------------------------------------------------------
-- Window rules:

-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ resource  =? "desktop_window" --> doIgnore
    , resource  =? "kdesktop"       --> doIgnore
    , className =? "feh"            --> doFloat
    , className =? "Gimp"           --> doFloat
    , className =? "Firefox"        --> doShift "2A"
    , className =? "firefox"        --> doShift "2A"
    , className =? "vlc"            --> doShift "4A"
    , className =? "Google-chrome"  --> doShift "5A"
    , className =? "Chromium"       --> doShift "5A"
    , className =? "Siki"           --> doShift "8A"
    , className =? "Spotify"        --> doShift "7B"
    -- 新しいウィンドウを末尾に追加しフォーカスする
    , not <$> isFloat               --> insertPosition End Newer
    ]
  where
    isFloat = liftX $ do
      floats <- gets $ W.floating . windowset
      ws <- gets $ W.integrate' . W.stack . W.workspace . W.current . windowset
      return $ any (flip M.member floats) ws

------------------------------------------------------------------------
-- Event handling

-- * EwmhDesktops users should change this to ewmhDesktopsEventHook
--
-- Defines a custom handler function for X Events. The function should
-- return (All True) if the default handler is to be run afterwards. To
-- combine event hooks use mappend or mconcat from Data.Monoid.
--
myEventHook = mempty

------------------------------------------------------------------------
-- Status bars and logging

-- Perform an arbitrary action on each internal state change or X event.
-- See the 'XMonad.Hooks.DynamicLog' extension for examples.
--
myLogHook = return ()
-- myLogHook = fadeOrigLogHook fadeAmount
--   where
--     fadeAmount = 0.8
--     fadeOrigLogHook = fadeOutLogHook . fadeIf fadeSomeWindows
--     fadeSomeWindows = className =? "URxvt"

myXmobarPP = xmobarPP
  {
    ppCurrent           =   xmobarColor "#d0d293" "#1a1a1a" . myDispF
  , ppVisible           =   xmobarColor "#e5e5e5" "#1a1a1a" . myDispF
  , ppHidden            =   xmobarColor "#6D9CBE" "#1a1a1a" . myDispF
  , ppHiddenNoWindows   =   xmobarColor "#444444" "#1a1a1a" . myDispF
  , ppUrgent            =   xmobarColor "red"     "#1a1a1a"
  , ppSep               =   "   "
  , ppWsSep             =   ""
  , ppTitle             =   xmobarColor "#e5e5e5" "#1a1a1a" . shortenB 30
  -- , ppTitleSanitize     =
  , ppLayout            =   xmobarColor "#6D9CBE" "#1a1a1a"
  -- , ppOrder             =
  , ppSort              =   getSortByTag
  -- , ppExtras            =
  -- , ppOutput            =
  }
  where
    myDispF (_:'A':[]) = "▲"
    myDispF (_:'B':[]) = "▼"
    shortenB n []      = []
    shortenB n (x:xs)
      | n > 0     = x : shortenB (n - if isAscii x then 1 else 2) xs
      | otherwise = "..."

------------------------------------------------------------------------
-- Startup hook

-- Perform an arbitrary action each time xmonad starts or is restarted
-- with mod-q.  Used by, e.g., XMonad.Layout.PerWorkspace to initialize
-- per-workspace layout choices.
--
-- By default, do nothing.
-- myStartupHook = return ()
myStartupHook = do
  spawn "xset s off -dpms"  -- Screensaver off, DPMS diable
  spawn "xmodmap ~/.Xmodmap"
  spawn "ibus-daemon -drx"
  -- feh
  bgDir       <- liftIO $ (++ "/data/pic") <$> getHomeDirectory
  bgDefault   <- liftIO $ head . (++ [bgDir ++ "/desktop.jpg"]) <$> (filterM doesFileExist $ map (\ext -> bgDir ++ "/desktop." ++ ext) ["png"])
  bgLeftPath  <- liftIO $ head . (++ [bgDefault]) <$> (filterM doesFileExist $ map (\ext -> bgDir ++ "/desktop_left." ++ ext) ["png", "jpg"])
  bgRightPath <- liftIO $ head . (++ [bgDefault]) <$> (filterM doesFileExist $ map (\ext -> bgDir ++ "/desktop_right." ++ ext) ["png", "jpg"])
  spawn $ "feh --bg-fill " ++ bgLeftPath ++ " --bg-fill " ++ bgRightPath
  -- compton
  spawn "compton -c -r 2 -o 0.8 -l -2 -t -2"
  --spawn "xscreensaver"
  spawn "~/bin/run-urxvtd.sh"
  --spawn "mlterm --daemon=genuine -e ':'" -- daemonだとemacsでctrl+F2が効かない
  setWMName "LG3D"  -- for java apps

------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.

-- Run xmonad with the settings you specify. No need to modify this.
--
--main = xmonad =<< xmobar defaults
--main = xmonad defaults
main = do
  -- The main function.
  --xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig
  xmonad . ewmh . docks .ewmhFullscreen $ myConfig
  where
    -- Command to launch the bar.
    myBar = "/usr/bin/xmobar"
    -- Custom PP, configure it as you like. It determines what is being written to the bar.
    -- myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "<" ">" }
    myPP = myXmobarPP
    -- Key binding to toggle the gap for the bar.
    toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)
    -- Main configuration, override the defaults to your liking.
    -- myConfig = defaultConfig { modMask = mod4Mask }
    myConfig = defaults

-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = def {
      -- simple stuff
        terminal           = myTerminal,
        focusFollowsMouse  = myFocusFollowsMouse,
        clickJustFocuses   = myClickJustFocuses,
        borderWidth        = myBorderWidth,
        modMask            = myModMask,
        workspaces         = myWorkspaces,
        normalBorderColor  = myNormalBorderColor,
        focusedBorderColor = myFocusedBorderColor,

      -- key bindings
        keys               = myKeys,
        mouseBindings      = myMouseBindings,

      -- hooks, layouts
        layoutHook         = myLayout,
        manageHook         = myManageHook,
        handleEventHook    = myEventHook,
        logHook            = myLogHook,
        startupHook        = myStartupHook
    }

-- | Finally, a copy of the default bindings in simple textual tabular format.
help :: String
help = "https://wiki.haskell.org/File:Xmbindings.png"
