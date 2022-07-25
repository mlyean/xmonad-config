import           XMonad
import           XMonad.Actions.Submap
import           XMonad.Actions.WindowGo
import           XMonad.Hooks.DynamicLog
import           XMonad.Hooks.EwmhDesktops as E
import           XMonad.Hooks.ManageDocks
import           XMonad.Layout.ComboP
import           XMonad.Layout.Fullscreen as F
import           XMonad.Layout.MultiToggle
import           XMonad.Layout.MultiToggle.Instances
import           XMonad.Layout.NoBorders
import           XMonad.Layout.PerWorkspace
import           XMonad.Layout.Spacing
import           XMonad.Layout.Tabbed
import           XMonad.Layout.TwoPanePersistent
import           XMonad.Util.EZConfig
import           XMonad.Util.Ungrab
import           Data.Map.Strict as M
import           Data.ByteString.UTF8
import           Data.ByteString.ShellEscape
import           System.Exit
import           Graphics.X11.ExtraTypes.XF86

polybar :: XConfig l -> XConfig l
polybar conf =
  conf { logHook = do
           logHook conf
           dynamicLogWithPP
             $ def { ppCurrent = wrap "%{F#00ff00}" "%{F-}"
                   , ppVisible = wrap "%{F#ffffff}" "%{F-}"
                   , ppHidden = wrap "%{F#ffffff}" "%{F-}"
                   , ppHiddenNoWindows = wrap "%{F#333333}" "%{F-}"
                   , ppUrgent = wrap "%{F#ff0000}" "%{F-}"
                   , ppSep = "%{F#888888} | %{F-}"
                   , ppTitle = shorten 48
                   , ppTitleSanitize = id
                   , ppOrder = \(ws:_:t:_) -> [ws, t]
                   , ppOutput = spawn
                       . ("polybar-msg action xmonad send " ++)
                       . toString
                       . bytes
                       . bash
                       . fromString
                   }
       , startupHook = do
           startupHook conf
           spawn "$HOME/.config/polybar/launch.sh"
       }

main :: IO ()
main = xmonad . ewmh . docks . polybar $ myConfig
  where
    modm = mod4Mask

    myTall = Tall 1 (3 / 100) (1 / 2)

    myTabbed = tabbed shrinkText
      $ def { activeColor = "#000000"
            , inactiveColor = "#000000"
            , urgentColor = "#000000"
            , activeBorderColor = "#000000"
            , inactiveBorderColor = "#000000"
            , urgentBorderColor = "#000000"
            , activeTextColor = "#00ff00"
            , inactiveTextColor = "#ffffff"
            , urgentTextColor = "#ff0000"
            , fontName = "xft:Fira Code:size=11"
            }

    defaultLayout =
      spacingRaw True (Border 6 6 6 6) False (Border 6 6 6 6) False myTall

    ws2Layout = noBorders myTabbed

    ws3Layout = combineTwoP
      (TwoPanePersistent Nothing 0 (1 / 4))
      Full
      myTabbed
      (ClassName "vifm")

    myLayoutHook = avoidStruts
      . fullscreenFull
      . mkToggle (NBFULL ?? EOT)
      . onWorkspace "2" ws2Layout
      . onWorkspace "3" ws3Layout
      . smartBorders
      $ defaultLayout

    myKeys =
      [ ((modm, xK_Return), spawn "kitty")
      , ((modm, xK_b), sendMessage ToggleStruts)
      , ((modm, xK_p), spawn "rofi -show drun")
      , ((modm, xK_q), kill)
      , ((modm, xK_f), sendMessage (Toggle NBFULL))
      , ( (modm, xK_g)
        , toggleWindowSpacingEnabled <+> toggleScreenSpacingEnabled)
      , ((modm, xK_s), sendMessage SwapWindow)
      , ( (modm, xK_x)
        , submap . M.fromList
          $ [ ((noModMask, xK_b), spawn "polybar-msg cmd restart")
            , ((noModMask, xK_l), spawn "loginctl lock-session")
            , ((noModMask, xK_o), spawn "systemctl poweroff")
            , ((noModMask, xK_q), io exitSuccess)
            , ((noModMask, xK_r), spawn "systemctl reboot")
            , ((noModMask, xK_x), spawn "xkill")])
      , ( (modm, xK_r)
        , submap . M.fromList
          $ [ ( (noModMask, xK_b)
              , runOrRaise "firefox" (className =? "firefox"))
            , ((noModMask, xK_d), spawn "discord")
            , ( (noModMask, xK_f)
              , raiseMaybe
                  (spawn "kitty --class vifm vifm")
                  (className =? "vifm"))
            , ( (shiftMask, xK_f)
              , runOrRaise "pcmanfm" (className =? "pcmanfm"))
            , ( (noModMask, xK_g)
              , raiseMaybe (spawn "kitty --class gp gp") (className =? "gp"))
            , ( (noModMask, xK_m)
              , runOrRaise "mathematica" (className =? "Mathematica"))
            , ( (noModMask, xK_n)
              , runOrRaise "notion-app" (className =? "notion-app"))
            , ( (noModMask, xK_s)
              , raiseMaybe
                  (spawn "kitty --class Singular Singular")
                  (className =? "Singular"))
            , ( (noModMask, xK_t)
              , runOrRaise "thunderbird" (className =? "Thunderbird"))])
      , ((modm, xK_z), spawn "polybar-msg action date toggle")
      , ((modm, xK_F6), spawn "pavucontrol")
      , ((noModMask, xF86XK_MonBrightnessUp), spawn "light -A 5")
      , ((noModMask, xF86XK_MonBrightnessDown), spawn "light -U 5")
      , ((modm, xK_F2), spawn "redshift -P -O 2500")
      , ((modm, xK_F3), spawn "redshift -x")
      , ( (noModMask, xF86XK_AudioMute)
        , spawn "amixer -D pulse sset Master toggle")
      , ( (noModMask, xF86XK_AudioLowerVolume)
        , spawn "amixer -D pulse sset Master 5%-")
      , ( (noModMask, xF86XK_AudioRaiseVolume)
        , spawn "amixer -D pulse sset Master 5%+")
      , ((noModMask, xF86XK_AudioPrev), spawn "playerctl previous")
      , ((noModMask, xF86XK_AudioPlay), spawn "playerctl play-pause")
      , ((noModMask, xF86XK_AudioNext), spawn "playerctl next")
      , ( (noModMask, xK_Print)
        , spawn "scrot $HOME/Pictures/screenshots/scrot-%Y-%m-%d-%H%M%S.png")
      , ( (controlMask, xK_Print)
        , unGrab
          >> spawn
            "scrot -f -s $HOME/Pictures/screenshots/scrot-%Y-%m-%d-%H%M%S.png")]

    myConfig =
      def { normalBorderColor = "#888888"
          , focusedBorderColor = "#ff00ff"
          , terminal = "kitty"
          , layoutHook = myLayoutHook
          , manageHook = fullscreenManageHook
          , handleEventHook = handleEventHook def <+> F.fullscreenEventHook
          , modMask = modm
          , borderWidth = 1
          , focusFollowsMouse = False
          , clickJustFocuses = False
          }
      `additionalKeys` myKeys
      `removeKeys` [ (modm .|. shiftMask, xK_Return)
                   , (modm .|. shiftMask, xK_c)
                   , (modm .|. shiftMask, xK_q)]
