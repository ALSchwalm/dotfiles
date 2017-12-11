{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses, PatternGuards, TypeSynonymInstances, DeriveDataTypeable #-}

import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.SetWMName
import XMonad.Actions.CycleWS
import XMonad.Actions.CopyWindow
import XMonad.Actions.SpawnOn
import XMonad.Actions.WindowGo
import XMonad.Layout.Tabbed
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Util.Run(safeSpawn)
import XMonad.Util.EZConfig(additionalKeys)
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import Control.Monad

-- The main function.
main :: IO()
main = xmonad =<<  statusBar myBar myPP  toggleStrutsKey (withUrgencyHook NoUrgencyHook myConfig)

-- Float specific windows
myManageHook :: ManageHook
myManageHook = composeAll
    [ className =? "MPlayer"   --> doFloat
    , isDialog                 --> doFloat
    , isFullscreen             --> (doF W.focusDown <+> doFullFloat)
    , manageWeb
    , manageSteam
    , manageTerm
    , manageIDA
    , manageEmacs]

manageWeb :: ManageHook
manageWeb = composeOne
    [ appName =? c -?> (doShift "3:web")
    | c <- [ "chromium",
             "google-chrome-stable",
             "firefox"
           ]]

manageIDA :: ManageHook
manageIDA = composeOne
    [ className =? "IDA" -?> doShift "7:IDA" ]

manageEmacs :: ManageHook
manageEmacs = composeOne
    [ appName =? "emacs" -?> doShift "2:emacs" ]

manageTerm :: ManageHook
manageTerm = composeAll
    [ (appName =? "xfce4-terminal") --> (doF W.swapDown)]

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
               , "7:IDA"
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
                  ppTitle = xmobarColor "#ee9a00" "" . shorten 50,
                  ppUrgent = wrap "~" "~"
                  }

myStartup :: X ()
myStartup = do
          raiseMaybe (spawnOn (myWorkspaces!!2) "firefox") (appName =? "firefox")
          raiseMaybe (spawn "emacs") (appName =? "emacs")
          replicateM_ 3 $ raiseMaybe (spawnOn (myWorkspaces!!0) "xfce4-terminal") (appName =? "xfce4-terminal")
          setWMName "Xfwm4"
          spawn "sh ~/.xmonad/run.sh"

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_y)

myLayouts = smartBorders  $ onWorkspace "8:steam" Full $
            tiled |||
            noBorders simpleTabbedBottom |||
            Full
  where
     tiled   = RotTall nmaster delta ratio
     nmaster = 1
     ratio   = 1/2
     delta   = 3/100


mouseResizeMinWindow w mx my = whenX (isClient w) $ withDisplay $ \d -> do
    io $ raiseWindow d w
    wa <- io $ getWindowAttributes d w
    sh <- io $ getWMNormalHints d w
    mouseDrag (\ex ey -> do
                 let x = ex - fromIntegral (wa_x wa)
                     y = ey - fromIntegral (wa_y wa)
                     sz = (max x mx, max y my)
                 io $ resizeWindow d w `uncurry`
                    applySizeHintsContents sh sz)
              (float w)

myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ ((modMask .|. controlMask, button1), mouseMoveWindow)
    , ((modMask, button3), (\w -> focus w >> windows W.swapMaster))
    , (((modMask .|. shiftMask), button1), (\w -> focus w >> mouseResizeMinWindow w 200 200)) ]

myConfig = defaultConfig
        { modMask = mod4Mask
        , manageHook = manageSpawn <+> manageDocks <+> myManageHook <+> manageSpawn <+> manageHook defaultConfig
        , layoutHook = avoidStruts $ myLayouts
        , startupHook = myStartup
        , handleEventHook = ewmhDesktopsEventHook
        , borderWidth = 3
        , focusedBorderColor = "#BBBBBB"
        , normalBorderColor = "#000000"
        , terminal = "xfce4-terminal"
        , workspaces = myWorkspaces
        , mouseBindings  = myMouseBindings
        } `additionalKeys`
        ([((0, 0x1008ff12),                 safeSpawn "pactl" ["set-sink-mute", "0", "toggle"])
        , ((0, 0x1008ff11),                 safeSpawn "amixer" ["-q", "set", "Master", "5%-"])
        , ((0, 0x1008ff13),                 safeSpawn "amixer" ["-q", "set", "Master", "5%+"])
        , ((mod4Mask , xK_Down ),           safeSpawn "amixer" ["-q", "set", "Master", "5%-"])
        , ((mod4Mask , xK_Up),              safeSpawn "amixer" ["-q", "set", "Master", "5%+"])
        , ((0, 0x1008ff1b),                 safeSpawn "toggle-brightness" [])
        , ((mod4Mask .|. shiftMask, xK_x),  safeSpawn "xkill" [])
        , ((mod4Mask , xK_e),               raiseMaybe (moveTo Next (WSIs $ return (("2:emacs" ==) . W.tag)) >>
                                                        spawn "emacs") (appName =? "emacs"))
        , ((mod4Mask , xK_u),               safeSpawn "google-chrome-stable" ["--high-dpi-support=1", "--force-device-scale-factor=1.21"])
        , ((mod4Mask .|. shiftMask, xK_u),  safeSpawn "google-chrome-stable" ["--high-dpi-support=1", "--force-device-scale-factor=1.21", "--incognito"])
        , ((mod4Mask , xK_F2),              safeSpawn "xbacklight" ["-dec", "3"])
        , ((mod4Mask , xK_F3),              safeSpawn "xbacklight" ["-inc", "3"])
        , ((0 , 0x1008ff03),                safeSpawn "xbacklight" ["-dec", "3"])
        , ((0 , 0x1008ff02),                safeSpawn "xbacklight" ["-inc", "3"])
        , ((mod4Mask , xK_p),               safeSpawn "rofi" ["-show", "run", "-location", "2", "-width", "100"])
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
        ++
        [((mod4Mask, k), smartWindows $ W.greedyView i)
           | (i, k) <- zip myWorkspaces [xK_1 .. xK_9]])

-- ^ Like 'windows' but does nothing if there is no change in window set
smartWindows :: (WindowSet -> WindowSet) -> X ()
smartWindows f = do
  XState { windowset = old } <- get
  let new = f old::WindowSet
  let windowid w = (W.tag ( W.workspace (W.current w)))
  if windowid old == windowid new
    then return ()
  else
    windows f

-- ^ Essentially the same layout as Tall, but the window order is better
data RotTall a = RotTall { tallNMaster :: !Int
                         , tallRatioIncrement :: !Rational
                         , tallRatio :: !Rational
                         }
               deriving (Show, Read)

-- a nice pure layout, lots of properties for the layout, and its messages, in Properties.hs
instance LayoutClass RotTall a where
    pureLayout (RotTall nmaster _ frac) r s = zip ws rs
      where ws = fst p ++ (reverse $ snd p)
              where p = splitAt nmaster $ W.integrate s
            rs = tile frac r nmaster (length ws)


    pureMessage (RotTall nmaster delta frac) m =
            msum [fmap resize     (fromMessage m)
                 ,fmap incmastern (fromMessage m)]

      where resize Shrink             = RotTall nmaster delta (max 0 $ frac-delta)
            resize Expand             = RotTall nmaster delta (min 1 $ frac+delta)
            incmastern (IncMasterN d) = RotTall (max 0 (nmaster+d)) delta frac

    description _ = "RotTall"
