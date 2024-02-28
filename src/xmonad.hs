{-# LANGUAGE TupleSections #-}

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
import           Data.Bifunctor (first)

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

withModMask :: KeyMask -> KeySym -> (KeyMask, KeySym)
withModMask m k = (m, k)

mapWithModMask :: KeyMask -> [(KeySym, X ())] -> [((KeyMask, KeySym), X ())]
mapWithModMask m = map (first (m, ))

submapList :: [((KeyMask, KeySym), X ())] -> X ()
submapList = submap . M.fromList

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

    mediaKeys =
      ( (modm, xK_m)
      , submapList $ mapWithModMask noModMask (zip mediaKeys2 mediaActions))
      :mapWithModMask noModMask (zip mediaKeys1 mediaActions)
      where
        mediaKeys1 = [ xF86XK_AudioPlay
                     , xF86XK_AudioPause
                     , xF86XK_AudioStop
                     , xF86XK_AudioPrev
                     , xF86XK_AudioNext]

        mediaKeys2 = [xK_p, xK_p, xK_s, xK_b, xK_n]

        playerctl s = safeSpawn "playerctl" [s]

        mediaActions = [ playerctl "play-pause"
                       , playerctl "play-pause"
                       , playerctl "stop"
                       , playerctl "previous"
                       , playerctl "next"]

    audioKeys = ((modm, xK_a), safeSpawnProg "pavucontrol")
      :mapWithModMask
        noModMask
        [ ( xF86XK_AudioMute
          , wpctl ["set-mute", "@DEFAULT_AUDIO_SINK@", "toggle"])
        , ( xF86XK_AudioLowerVolume
          , wpctl ["set-volume", "@DEFAULT_AUDIO_SINK@", "5%-"])
        , ( xF86XK_AudioRaiseVolume
          , wpctl ["set-volume", "-l", "1.5", "@DEFAULT_AUDIO_SINK@", "5%+"])]
      where
        wpctl = safeSpawn "wpctl"

    brightnessKeys = mapWithModMask
      noModMask
      [ (xF86XK_MonBrightnessUp, light ["-A", "5"])
      , (xF86XK_MonBrightnessDown, light ["-U", "5"])]
      where
        light = safeSpawn "light"

    displayKeys = mapWithModMask
      noModMask
      [ (xK_a, autorandr ["--change", "--force"])
      , (xK_m, autorandr ["--load", "mobile"])
      , (xK_e, autorandr ["--load", "external-only"])
      , (xK_d, autorandr ["--load", "docked"])]
      where
        autorandr x = spawn "xrandr --auto" >> safeSpawn "autorandr" x

    screenshotKeys =
      [ ( (noModMask, xK_Print)
        , spawn "scrot $HOME/Pictures/screenshots/scrot-%Y-%m-%d-%H%M%S.png")
      , ( (controlMask, xK_Print)
        , unGrab
          >> spawn
            "scrot -f -s $HOME/Pictures/screenshots/scrot-%Y-%m-%d-%H%M%S.png")]

    myKeys = mapWithModMask
      modm
      [ (xK_Return, safeSpawnProg "kitty")
      , (xK_b, sendMessage ToggleStruts)
      , (xK_p, safeSpawn "rofi" ["-show", "drun", "-monitor", "-1"])
      , (xK_q, kill)
      , (xK_f, sendMessage (Toggle NBFULL))
      , (xK_g, toggleWindowSpacingEnabled <+> toggleScreenSpacingEnabled)
      , (xK_s, sendMessage SwapWindow)
      , ( xK_x
        , submapList . mapWithModMask noModMask
          $ [ (xK_b, safeSpawn "polybar-msg" ["cmd", "restart"])
            , (xK_l, safeSpawn "loginctl" ["lock-session"])
            , (xK_o, safeSpawn "systemctl" ["poweroff"])
            , (xK_m, submapList displayKeys)
            , (xK_q, io exitSuccess)
            , (xK_r, safeSpawn "systemctl" ["reboot"])
            , (xK_x, safeSpawnProg "xkill")])
      , ( xK_r
        , submapList
          $ mapWithModMask
            noModMask
            [ (xK_b, runOrRaise "firefox" (className =? "firefox"))
            , (xK_d, safeSpawnProg "discord")
            , ( xK_f
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "vifm", "vifm"])
                  (className =? "vifm"))
            , ( xK_p
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "gp", "gp"])
                  (className =? "gp"))
            , ( xK_g
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "GAP", "gap"])
                  (className =? "GAP"))
            , (xK_m, runOrRaise "mathematica" (className =? "Mathematica"))
            , (xK_n, runOrRaise "notion-app" (className =? "notion-app"))
            , ( xK_s
              , raiseMaybe
                  (safeSpawn "kitty" ["--class", "Singular", "Singular"])
                  (className =? "Singular"))
            , (xK_t, runOrRaise "thunderbird" (className =? "Thunderbird"))]
          ++ [ ( (shiftMask, xK_f)
               , runOrRaise "pcmanfm" (className =? "pcmanfm"))])
      , (xK_z, safeSpawn "polybar-msg" ["action", "date", "toggle"])]
      ++ mediaKeys
      ++ audioKeys
      ++ brightnessKeys
      ++ screenshotKeys

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
