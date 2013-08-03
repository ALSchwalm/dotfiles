import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.FlexibleResize as Flex
import XMonad.Actions.SpawnOn
import XMonad.Layout.Tabbed
import XMonad.Layout.ResizableTile
import XMonad.Layout.NoBorders
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.Run(safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Data.List 
import System.IO

defaultModMask :: KeyMask
defaultModMask = mod4Mask

-- The main function.
main = xmonad =<< statusBar myBar myPP  toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Float specific windows
myManageHook = composeAll
    [ className =? "MPlayer"        --> doFloat
    , className =? "Gimp"           --> doFloat
    , isFullscreen 		    --> (doF W.focusDown <+> doFullFloat)
    , manageTerm
    , manageWeb
    , manageEmacs
    , manageSteam]


manageTerm :: ManageHook
manageTerm = composeOne
    [ className =? c -?> (ask >>= doF . \w -> (copyWindow w "1:term"))
    | c <- [ "xterm",
             "Xfce4-terminal"]]

manageEmacs :: ManageHook
manageEmacs = composeOne
    [ className =? c -?> (ask >>= doF . \w -> (copyWindow w "2:emacs"))
    | c <- [ "Emacs" ]]


manageWeb :: ManageHook
manageWeb = composeOne
    [ className =? c -?> (ask >>= doF . \w -> (copyWindow w "3:web"))
    | c <- [ "Chromium-browser",
             "Google-chrome",
             "Firefox"
           ]]

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
               , "7"
               , "8:steam"
               , "9:music"
               ]
    
-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#429942" "" . wrap "[" "]",
       		  ppLayout = const "",
                  ppTitle = const ""
                  }

myStartup = do
          spawnOn (myWorkspaces!!0) "xfce4-terminal"
                  
-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_y)


myLayouts = smartBorders tiled ||| simpleTabbedBottom ||| Full ||| Mirror tiled
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
	, manageHook = manageDocks <+> myManageHook <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ myLayouts
        , startupHook = myStartup
	, focusedBorderColor = "#555555"
	, normalBorderColor = "#000000"
        , terminal = "xfce4-terminal"
        , workspaces = myWorkspaces
        , mouseBindings  = myMouseBindings
        } `additionalKeys`
        [ ((mod4Mask , xK_F8 ),             safeSpawn "amixer" ["-q", "set", "Master", "toggle"])
        , ((mod4Mask , xK_F9 ),             safeSpawn "amixer" ["-q", "set", "Master", "5-"])
        , ((mod4Mask , xK_F10),             safeSpawn "amixer" ["-q", "set", "Master", "5+"])
	, ((mod4Mask , xK_Down ),           safeSpawn "amixer" ["-q", "set", "Master", "5-"])
	, ((mod4Mask , xK_Up),              safeSpawn "amixer" ["-q", "set", "Master", "5+"])
        , ((mod4Mask , xK_e),               safeSpawn "emacsclient" ["-c"])
        , ((mod4Mask , xK_u),               safeSpawn "google-chrome" [])
        , ((mod4Mask , xK_F6),              safeSpawn "brightness" ["down"])
        , ((mod4Mask , xK_F7),              safeSpawn "brightness" ["up"])
        , ((mod4Mask , xK_f) ,              nextWS)
        , ((mod4Mask , xK_Right) ,          nextWS)
        , ((mod4Mask .|. shiftMask, xK_f) , shiftToNext)
        , ((mod4Mask , xK_b) ,              prevWS)
        , ((mod4Mask , xK_Left) ,           prevWS)
        , ((mod4Mask .|. shiftMask, xK_b) , shiftToPrev)
        , ((mod4Mask , xK_w) ,              moveTo Next EmptyWS)
        , ((mod4Mask .|. shiftMask, xK_w) , shiftTo Next EmptyWS)
        , ((mod4Mask , xK_Tab) ,            toggleWS)
        ]