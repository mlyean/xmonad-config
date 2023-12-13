import qualified Data.Map.Strict as M
import           Graphics.X11.ExtraTypes.XF86
import           System.Exit (exitSuccess)
import           XMonad
import           XMonad.Actions.CopyWindow (copyToAll)
import           XMonad.Actions.Submap (submap)
import           XMonad.Actions.WindowGo (runOrRaise, raiseMaybe)
import           XMonad.Hooks.DynamicLog
import qualified XMonad.Hooks.EwmhDesktops as E
import           XMonad.Hooks.ManageDocks
import           XMonad.Hooks.WindowSwallowing (swallowEventHook)
import           XMonad.Layout.ComboP
import qualified XMonad.Layout.Fullscreen as F
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders (noBorders, smartBorders)
import           XMonad.Layout.PerWorkspace (onWorkspace)
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPanePersistent
import           XMonad.Util.EZConfig (additionalKeys, removeKeys)
import           XMonad.Util.Run (safeSpawn, safeSpawnProg)
import           XMonad.Util.Ungrab (unGrab)
import           XMonad.Util.WindowProperties (getProp32)

polybar :: XConfig l -> XConfig l
polybar conf =
  conf { logHook = do
           logHook conf
           dynamicLogWithPP
             $ def { ppCurrent = wrap
                       "%{F#ff79c6}%{B#44475a}%{u#bd93f9}%{+u} "
                       " %{F- B- -u}"
                   , ppVisible = wrap "%{F#ff79c6} " " %{F-}"
                   , ppHidden = wrap "%{F#f8f8f2} " " %{F-}"
                   , ppHiddenNoWindows = const ""
                   , ppUrgent = wrap "%{F#ff5555} " " %{F-}"
                   , ppWsSep = ""
                   , ppTitle = wrap "%{F#f8f8f2}" "%{F-}" . shorten 48
                   , ppOrder = take 1
                   , ppOutput = \x -> safeSpawn
                       "polybar-msg"
                       ["action", "xmonad", "send", x]
                   }
       , startupHook = do
           startupHook conf
           spawn "$HOME/.config/polybar/launch.sh"
       }

getNetWMState :: Window -> X [Atom]
getNetWMState w = do
  atom <- getAtom "_NET_WM_STATE"
  maybe [] (map fromIntegral) <$> getProp32 atom w

hasNetWMState :: String -> Query Bool
hasNetWMState st = do
  window <- ask
  wmstate <- liftX $ getNetWMState window
  atom <- liftX $ getAtom st
  return $ elem atom wmstate

main :: IO ()
main = xmonad . E.ewmh . docks . polybar $ myConfig
  where
    modm = mod4Mask

    myTall = Tall 1 (3 / 100) (1 / 2)

    myTabbed = tabbed shrinkText
      $ def { activeColor = "#282a36"
            , inactiveColor = "#282a36"
            , urgentColor = "#282a36"
            , activeBorderColor = "#282a36"
            , inactiveBorderColor = "#282a36"
            , urgentBorderColor = "#282a36"
            , activeTextColor = "#bd93f9"
            , inactiveTextColor = "#f8f8f2"
            , urgentTextColor = "#ff5555"
            , fontName = "xft:Fira Code:size=12"
            }

    defaultLayout =
      spacingRaw True (Border 10 10 10 10) False (Border 10 10 10 10) False
      $ myTall

    ws2Layout = noBorders myTabbed

    ws3Layout = combineTwoP
      (TwoPanePersistent Nothing 0 (1 / 4))
      Full
      myTabbed
      (ClassName "vifm")

    myLayoutHook = avoidStruts
      . F.fullscreenFull
      . mkToggle (NBFULL ?? EOT)
      . onWorkspace "2" ws2Layout
      . onWorkspace "3" ws3Layout
      . smartBorders
      $ defaultLayout

    myKeys =
      [ ((modm, xK_Return), safeSpawnProg "kitty")
      , ((modm, xK_b), sendMessage ToggleStruts)
      , ((modm, xK_p), safeSpawn "rofi" ["-show", "drun", "-monitor", "-1"])
      , ((modm, xK_q), kill)
      , ((modm, xK_f), sendMessage (Toggle NBFULL))
      , ( (modm, xK_g)
        , toggleWindowSpacingEnabled <+> toggleScreenSpacingEnabled)
      , ((modm, xK_s), sendMessage SwapWindow)
      , ( (modm, xK_x)
        , submap . M.fromList
          $ [ ((noModMask, xK_b), safeSpawn "polybar-msg" ["cmd", "restart"])
            , ((noModMask, xK_l), safeSpawn "loginctl" ["lock-session"])
            , ((noModMask, xK_o), safeSpawn "systemctl" ["poweroff"])
            , ( (noModMask, xK_m)
              , safeSpawn "autorandr" ["--change", "--force"])
            , ((noModMask, xK_q), io exitSuccess)
            , ((noModMask, xK_r), safeSpawn "systemctl" ["reboot"])
            , ((noModMask, xK_x), safeSpawnProg "xkill")])
      , ( (modm, xK_r)
        , submap . M.fromList
          $ [ ( (noModMask, xK_b)
              , runOrRaise "firefox" (className =? "firefox"))
            , ((noModMask, xK_d), safeSpawnProg "discord")
            , ( (noModMask, xK_f)
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "vifm", "vifm"])
                  (className =? "vifm"))
            , ( (shiftMask, xK_f)
              , runOrRaise "pcmanfm" (className =? "pcmanfm"))
            , ( (noModMask, xK_p)
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "gp", "gp"])
                  (className =? "gp"))
            , ( (noModMask, xK_g)
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "GAP", "gap"])
                  (className =? "GAP"))
            , ( (noModMask, xK_m)
              , runOrRaise "mathematica" (className =? "Mathematica"))
            , ( (noModMask, xK_n)
              , runOrRaise "notion-app" (className =? "notion-app"))
            , ( (noModMask, xK_s)
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "Singular", "Singular"])
                  (className =? "Singular"))
            , ( (noModMask, xK_t)
              , runOrRaise "thunderbird" (className =? "Thunderbird"))])
      , ((modm, xK_z), safeSpawn "polybar-msg" ["action", "date", "toggle"])
      , ((modm, xK_a), safeSpawnProg "pavucontrol")
      , ((noModMask, xF86XK_MonBrightnessUp), safeSpawn "light" ["-A", "5"])
      , ((noModMask, xF86XK_MonBrightnessDown), safeSpawn "light" ["-U", "5"])
      , ((modm, xK_F5), safeSpawn "redshift" ["-P", "-O", "2500"])
      , ((modm, xK_F6), safeSpawn "redshift" ["-x"])
      , ( (noModMask, xF86XK_AudioMute)
        , safeSpawn "wpctl" ["set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"])
      , ( (noModMask, xF86XK_AudioLowerVolume)
        , safeSpawn "wpctl" ["set-volume", "@DEFAULT_AUDIO_SINK@", "5%-"])
      , ( (noModMask, xF86XK_AudioRaiseVolume)
        , safeSpawn
            "wpctl"
            ["set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", "5%+"])
      , ((noModMask, xF86XK_AudioStop), safeSpawn "playerctl" ["stop"])
      , ((noModMask, xF86XK_AudioPrev), safeSpawn "playerctl" ["previous"])
      , ((noModMask, xF86XK_AudioPlay), safeSpawn "playerctl" ["play-pause"])
      , ((noModMask, xF86XK_AudioPause), safeSpawn "playerctl" ["play-pause"])
      , ((noModMask, xF86XK_AudioNext), safeSpawn "playerctl" ["next"])
      , ( (modm, xK_m)
        , submap . M.fromList
          $ [ ((noModMask, xK_p), safeSpawn "playerctl" ["play-pause"])
            , ((noModMask, xK_b), safeSpawn "playerctl" ["previous"])
            , ((noModMask, xK_n), safeSpawn "playerctl" ["next"])
            , ((noModMask, xK_s), safeSpawn "playerctl" ["stop"])])
      , ( (noModMask, xK_Print)
        , spawn "scrot $HOME/Pictures/screenshots/scrot-%Y-%m-%d-%H%M%S.png")
      , ( (controlMask, xK_Print)
        , unGrab
          >> spawn
            "scrot -f -s $HOME/Pictures/screenshots/scrot-%Y-%m-%d-%H%M%S.png")]

    myConfig =
      def { normalBorderColor = "#6272a4"
          , focusedBorderColor = "#bd93f9"
          , terminal = "kitty"
          , layoutHook = myLayoutHook
          , manageHook = composeAll
              [ F.fullscreenManageHook
              , hasNetWMState "_NET_WM_STATE_ABOVE" --> doFloat
              , hasNetWMState "_NET_WM_STATE_STICKY" --> doF copyToAll
              , appName =? "Toolkit" --> doF copyToAll] -- Firefox PIP
          , handleEventHook = handleEventHook def
              <+> F.fullscreenEventHook
              <+> swallowEventHook
                (className =? "kitty")
                (className =? "mpv" <||> className =? "Zathura")
          , modMask = modm
          , borderWidth = 1
          , focusFollowsMouse = False
          , clickJustFocuses = False
          }
      `removeKeys` [ (modm .|. shiftMask, xK_Return)
                   , (modm .|. shiftMask, xK_c)
                   , (modm .|. shiftMask, xK_q)]
      `additionalKeys` myKeys
