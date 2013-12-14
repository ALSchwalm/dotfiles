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
               , "4:eclipse"
               , "5"
               , "6"
               , "7:irc"
               , "8:steam"
               , "9:music"
               ]
    
-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#ee9a00" "" . wrap "[" "]",
       		  ppLayout = const "",
                  ppTitle = const "",
                  ppUrgent = wrap "~" "~"
                  }

myLogHook :: X ()
myLogHook = fadeInactiveLogHook fadeAmount
     where fadeAmount = 0.8

myStartup = do
          spawnOn (myWorkspaces!!0) "xfce4-terminal"
          spawnOn (myWorkspaces!!2) "google-chrome"
          spawn "emacsclient -c -a ''"
          spawn "sh ~/.xmonad/run.sh"
          safeSpawn "xchat" []
                  
-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_y)


myLayouts = smartBorders  $ onWorkspace "8:steam" Full $ tiled |||  simpleTabbedBottom |||  Full ||| Mirror tiled
  where
     tiled   = Tall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    , (((modMask .|. shiftMask), button1), (\w -> focus w >> Flex.mouseResizeWindow w)) ]

myConfig = defaultConfig
        { modMask = mod4Mask
	, manageHook = manageDocks <+> myManageHook <+> manageSpawn <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ myLayouts
        , startupHook = myStartup
        , logHook = myLogHook
        , borderWidth = 3
	, focusedBorderColor = "#AA3333"
	, normalBorderColor = "#000000"
        , terminal = "xfce4-terminal"
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
                                                        safeSpawn "emacsclient" ["-c"]) (className =? "Emacs24"))
        , ((mod4Mask , xK_g),               goToSelected defaultGSConfig)  
        , ((mod4Mask , xK_u),               safeSpawn "google-chrome" [])
        , ((0, 0x1008ff03),                 safeSpawn "brightness" ["-0.1"])
        , ((0, 0x1008ff02),                 safeSpawn "brightness" ["+0.1"])
        , ((mod4Mask , xK_f) ,              nextWS)
        , ((mod4Mask , xK_Right) ,          nextWS)
        , ((mod4Mask , xK_d) ,              windows copyToAll)
        , ((mod4Mask .|. shiftMask, xK_d) , killAllOtherCopies)
        , ((mod4Mask .|. shiftMask, xK_f) , shiftToNext)
        , ((mod4Mask , xK_b) ,              prevWS)
        , ((mod4Mask , xK_Left) ,           prevWS)
        , ((mod4Mask .|. shiftMask, xK_b) , shiftToPrev)
        , ((mod4Mask , xK_w) ,              moveTo Next EmptyWS)
        , ((mod4Mask .|. shiftMask, xK_w) , shiftTo Next EmptyWS)
        , ((mod4Mask , xK_Tab) ,            toggleWS)
        , ((mod1Mask , xK_Tab) ,            windows W.focusDown)
        , ((mod1Mask .|. shiftMask, xK_Tab) , windows W.focusUp)
        ]
