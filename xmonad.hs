import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.Run(safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Control.Monad
import Data.List 

-- The main function.
main = xmonad =<<  statusBar myBar myPP  toggleStrutsKey (withUrgencyHook NoUrgencyHook myConfig)

-- Float specific windows
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer"   --> doFloat
    , className =? "Gimp"      --> doFloat
    , isDialog                 --> doFloat
    , isFullscreen 	       --> (doF W.focusDown <+> doFullFloat)
    , manageWeb
    , manageSteam
    , manageTerm
    , manageIRC
    , manageEmacs]

manageWeb :: ManageHook
manageWeb = composeOne
    [ appName =? c -?> (doShift "3:web")
    | c <- [ "chromium-browser",
             "google-chrome-stable",
             "firefox"
           ]]

manageIRC :: ManageHook
manageIRC = composeOne
    [ className =? "Xchat" -?> doShift "7:irc" ]

manageEmacs :: ManageHook
manageEmacs = composeOne
    [ appName =? "emacs" -?> doShift "2:emacs" ]

manageTerm :: ManageHook
manageTerm = composeAll
    [ (appName =? "lxterminal") --> (doF W.swapDown)]

manageSteam :: ManageHook
manageSteam = composeAll
    [ className =? "Steam" --> doShift "8:steam",
      title =? "Friends"  --> doFloat ]

myWorkspaces :: [WorkspaceId]
myWorkspaces = [ "1:term"
               , "2:emacs"
               , "3:web"
               , "4"
               , "5"
               , "6"
               , "7:irc"
               , "8:steam"
               , "9:music"
               ]

-- Command to launch the bar.
myBar :: String
myBar = "xmobar"

-- Custom PP, configure it as you like. It determines what is being written to the bar
myPP :: PP
myPP = xmobarPP { ppCurrent = xmobarColor "#ee9a00" "" . wrap "[" "]",
       		  ppLayout = const "",
                  ppTitle = const "",
                  ppUrgent = wrap "~" "~"
                  }  

myLogHook :: X ()
myLogHook = ewmhDesktopsLogHook <+> fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.8

myStartup :: X ()
myStartup = do
          raiseMaybe (spawnOn (myWorkspaces!!2) "google-chrome-stable") (appName =? "google-chrome-stable")
          raiseMaybe (spawn "emacsclient -c -a ''") (appName =? "emacs")
          replicateM_ 3 $ raiseMaybe (spawnOn (myWorkspaces!!0) "lxterminal") (appName =? "lxterminal")
          spawn "sh ~/.xmonad/run.sh"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_y)

myLayouts = smartBorders  $ onWorkspace "8:steam" Full $
            tiled |||
            smartSpacing 10 tiled |||
            noBorders simpleTabbedBottom |||
            Full
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button3), (\w -> focus w >> windows W.swapMaster))
    , (((modMask .|. shiftMask), button1), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

myConfig = defaultConfig
        { modMask = mod4Mask
	, manageHook = manageSpawn <+> manageDocks <+> myManageHook <+> manageSpawn <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ myLayouts
        , startupHook = myStartup
        , handleEventHook = ewmhDesktopsEventHook
        , logHook = myLogHook
        , borderWidth = 3
	, focusedBorderColor = "#BBBBBB"
	, normalBorderColor = "#000000"
        , terminal = "lxterminal"
        , workspaces = myWorkspaces
        , mouseBindings  = myMouseBindings
        } `additionalKeys`
        [ ((0, 0x1008ff12),                 safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
        , ((0, 0x1008ff11),                 safeSpawn "amixer" ["-q", "set", "Master", "5-"])
        , ((0, 0x1008ff13),                 safeSpawn "amixer" ["-q", "set", "Master", "5+"])
	, ((mod4Mask , xK_Down ),           safeSpawn "amixer" ["-q", "set", "Master", "5-"])
	, ((mod4Mask , xK_Up),              safeSpawn "amixer" ["-q", "set", "Master", "5+"])
        , ((mod4Mask .|. shiftMask, xK_x),  safeSpawn "xkill" [])
        , ((mod4Mask , xK_e),               raiseMaybe (moveTo Next (WSIs $ return (("2:emacs" ==) . W.tag)) >> 
                                                        spawn "emacsclient -c -a ''") (appName =? "emacs"))
        , ((mod4Mask , xK_g),               goToSelected defaultGSConfig)  
        , ((mod4Mask , xK_u),               safeSpawn "google-chrome-stable" [])
        , ((mod4Mask .|. shiftMask, xK_u),  safeSpawn "google-chrome-stable" ["--incognito"])
        , ((0, 0x1008ff03),                 safeSpawn "brightness" ["-0.1"])
        , ((0, 0x1008ff02),                 safeSpawn "brightness" ["+0.1"])
        , ((mod4Mask , xK_r),               spawn "rotate toggle right")
        , ((mod4Mask .|. shiftMask, xK_r),  spawn "rotate toggle left")
        , ((mod4Mask , xK_f),               nextWS)
        , ((mod4Mask , xK_Right),           nextWS)
        , ((mod4Mask , xK_d),               windows copyToAll)
        , ((mod4Mask .|. shiftMask, xK_d),  killAllOtherCopies)
        , ((mod4Mask .|. shiftMask, xK_f),  shiftToNext)
        , ((mod4Mask , xK_b),               prevWS)
        , ((mod4Mask , xK_Left),            prevWS)
        , ((mod4Mask .|. shiftMask, xK_b) , shiftToPrev)
        , ((mod4Mask , xK_w),               moveTo Next EmptyWS)
        , ((mod4Mask .|. shiftMask, xK_w),  shiftTo Next EmptyWS)
        , ((mod4Mask , xK_Tab),             toggleWS)
        , ((mod1Mask , xK_Tab),             windows W.focusDown)
        , ((mod1Mask .|. shiftMask, xK_Tab),  windows W.focusUp)
        ]
