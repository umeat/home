import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Layout.NoBorders
import XMonad.Util.EZConfig (additionalKeysP)
import XMonad.Util.Run (spawnPipe)
import Graphics.X11.ExtraTypes.XF86
import System.IO (hPutStrLn)

main :: IO ()
main = do
    xmproc <- spawnPipe "xmobar"
    spawn "~/.fehbg"
    xmonad $ def
        { terminal = "xterm -sl 1000"
        , manageHook = manageDocks <+> manageHook def
        , layoutHook = avoidStruts $ smartBorders $ layoutHook def
        , startupHook = setWMName "LG3D" -- java swing
        , handleEventHook = handleEventHook def <+> docksEventHook
        , logHook = dynamicLogWithPP xmobarPP
            { ppOutput = hPutStrLn xmproc
            , ppTitle = xmobarColor "grey" "" . shorten 53
            , ppCurrent = wrap "<fc=#919191>[</fc>" "<fc=#919191>]</fc>" . xmobarColor "#E1E1E1" "" 
            }
        , focusedBorderColor = "#1d1f21"
        , normalBorderColor = "#000000"
        , focusFollowsMouse = False
        } `additionalKeysP`
        [
        ("M-p", spawn "dmenu_run -fn inconsolata-18")
        , ("M-'", sendMessage ToggleStruts)
        , ("<xF86AudioMute>", spawn "amixer -q set Master toggle")
        , ("<xF86AudioRaiseVolume>", spawn "amixer -q set Master 5%+")
        , ("<xF86AudioLowerVolume>", spawn "amixer -q set Master 5%-")
        , ("<xF86AudioMicMute>", spawn "amixer -q set Capture toggle")
        , ("<xF86MonBrightnessUp>", spawn "xbacklight -inc 10")
        , ("<xF86MonBrightnessDown>", spawn "xbacklight -dec 10")
        , ("<xPrint>", spawn "scrot /tmp/selection.png && xclip -selection clipboard -t image/png -i /tmp/selection.png")
        , ("M-<xPrint>", spawn "scrot -s /tmp/selection.png && xclip -selection clipboard -t image/png -i /tmp/selection.png")
        , ("M-l", spawn "sudo slock")
        ]
