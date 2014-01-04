import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.FadeInactive
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.SpawnOn
import XMonad.Actions.GridSelect
import XMonad.Actions.WindowGo
import XMonad.Layout.Spacing
import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.Run(spawnPipe, safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List 
import System.IO

defaultModMask :: KeyMask
defaultModMask = mod4Mask

-- The main function.
main = xmonad =<<  statusBar myBar myPP  toggleStrutsKey (withUrgencyHook NoUrgencyHook myConfig)

-- Command to launch the bar.
myBar = "xmobar"

-- Float specific windows
myManageHook = composeAll
    [ className =? "MPlayer"   --> doFloat
    , className =? "Gimp"      --> doFloat
    , isDialog                 --> doFloat
    , isFullscreen 	       --> (doF W.focusDown <+> doFullFloat)
    , manageWeb
    , manageSteam
    , manageTerm
    , manageIRC
    , manageEmacs
    , manageEclipse]

manageWeb :: ManageHook
manageWeb = composeOne
    [ className =? c -?> (doShift "3:web")
    | c <- [ "Chromium-browser",
             "Google-chrome",
             "Firefox"
           ]]

manageIRC :: ManageHook
manageIRC = composeOne
    [ className =? "Xchat" -?> doShift "7:irc" ]

manageEmacs :: ManageHook
manageEmacs = composeOne
    [ className =? "Emacs24" -?> doShift "2:emacs" ]

manageTerm :: ManageHook
manageTerm = composeAll
    [ (className =? "Xfce4-terminal") --> (doF W.swapDown)]

manageSteam :: ManageHook
manageSteam = composeAll
    [ className =? "Steam" --> doShift "8:steam",
      title =? "Friends"  --> doFloat ]

manageEclipse :: ManageHook
manageEclipse = composeAll
    [ className =? "Eclipse" --> doShift "4:eclipse" ]
      
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
    
-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#ee9a00" "" . wrap "[" "]",
       		  ppLayout = layoutName,
                  ppTitle = const "",
                  ppUrgent = wrap "~" "~"
                  } where
  layoutName x
    | "Smart" `isInfixOf` x = "SpTall"
    | "Tall"  `isInfixOf` x = "Tall"
    | "Grid"  `isInfixOf` x = "Grid"
    | "Tab"   `isInfixOf` x = "Tab"
    | "Full"  `isInfixOf` x = "Full"
    | otherwise = show x
    

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.8

myStartup = do
          raiseMaybe (spawnOn (myWorkspaces!!2) "google-chrome") (className =? "Google-chrome")
          raiseMaybe (spawn "emacsclient -c -a ''") (className =? "Emacs24")
          raiseMaybe (spawnOn (myWorkspaces!!0) "lxterminal") (className =? "lxterminal")
          spawn "sh ~/.xmonad/run.sh"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_y)


trayerCmd = concat ["trayer --transparent true",
                     " --heighttype pixel",
                     " --height 14",
                     " --tint 0x000000",
                     " --alpha 0",
                     " --widthtype pixel",
                     " --width 40",
                     " --align right",
                     " --distancefrom right",
                     " --distance 445",
                     " --expand false &"]


myLayouts = smartBorders  $ onWorkspace "8:steam" Full $
            smartSpacing 4 tiled |||
            tiled |||
            noBorders simpleTabbedBottom |||
            Full |||
            GridRatio (4/3)
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
                                                        safeSpawn "emacs" []) (className =? "Emacs24"))
        , ((mod4Mask , xK_g),               goToSelected defaultGSConfig)  
        , ((mod4Mask , xK_u),               safeSpawn "google-chrome" [])
        , ((mod4Mask .|. shiftMask, xK_u),  safeSpawn "google-chrome" ["--incognito"])
        , ((0, 0x1008ff03),                 safeSpawn "brightness" ["-0.1"])
        , ((0, 0x1008ff02),                 safeSpawn "brightness" ["+0.1"])
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
        , ((mod4Mask , xK_s),               spawn trayerCmd)
        , ((mod4Mask .|. shiftMask, xK_s),  spawn "killall trayer")
        ]
