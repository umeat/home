import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeysP, additionalKeys)
import XMonad.Util.Run (spawnPipe)
import System.IO (hPutStrLn)
import Graphics.X11.ExtraTypes.XF86
import qualified XMonad.Hooks.DynamicBars as Bars

barCreator :: Bars.DynamicStatusBar
barCreator (S sid) = spawnPipe $ "xmobar --screen " ++ show sid

barPP = xmobarPP 
    { ppTitle = xmobarColor "grey" "" . shorten 53
    , ppCurrent = wrap "<fc=#919191>[</fc>" "<fc=#919191>]</fc>" . xmobarColor "#E1E1E1" "" 
    }

main :: IO ()
main = do
    spawn "feh --bg-tile /home/brandon/.wallpaper.jpg"
    xmonad $ def
        { terminal = "urxvt"
        , manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts $ smartBorders $ layoutHook def
        , startupHook = do
            setWMName "LG3D" -- java swing
            Bars.dynStatusBarStartup barCreator (return ())
        , logHook = Bars.multiPP barPP barPP
        , handleEventHook = handleEventHook def <+> docksEventHook
        , focusedBorderColor = "#1d1f21"
        , normalBorderColor = "#000000"
        , focusFollowsMouse = False
        , modMask = mod4Mask -- Windows / Command key
        } `additionalKeysP`
            [ ("M-p", spawn "dmenu_run -fn inconsolata-18")
            , ("<XF86Open>", spawn "dmenu_run -fn inconsolata-18")
            , ("M-'", sendMessage ToggleStruts)
            , ("<XF86AudioMute>", spawn "amixer -q set Master toggle")
            , ("<XF86AudioRaiseVolume>", spawn "pactl set-sink-volume 0 +5%")
            , ("<XF86AudioLowerVolume>", spawn "pactl set-sink-volume 0 -5%")
            , ("<XF86AudioMicMute>", spawn "amixer -q set Capture toggle")
            , ("<XF86MonBrightnessUp>", spawn "sudo light -A 10")
            , ("<XF86MonBrightnessDown>", spawn "sudo light -U 10")
            , ("<Print>", spawn "scrot /tmp/selection.png && xclip -selection clipboard -t image/png -i /tmp/selection.png")
            , ("M-<Print>", spawn "scrot -s /tmp/selection.png && xclip -selection clipboard -t image/png -i /tmp/selection.png")
            , ("M-S-l", spawn "sudo slock")
            , ("M-n", spawn "kill $(pgrep notify-osd)")
            , ("C-q", spawn "") -- Disable ctrl-q for firefox ...
        ]
        `additionalKeys`
            [((0, xK_Find), spawn "dmenu_run -fn inconsolata-18")
        ]
