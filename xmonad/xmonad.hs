-- Based on:
-- xmonad-0.7 config-mmarx
-- (c) 2008 Maximilian Marx

module Main where

import System.Exit (exitSuccess)

import Data.Map (Map)

import XMonad hiding ((|||))

import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Grid (Grid(..))
import XMonad.Layout.LayoutCombinators (JumpToLayout(..), (|||))
import XMonad.Layout.MultiColumns (multiCol)
import XMonad.Layout.TwoPane (TwoPane(..))

import XMonad.Hooks.EwmhDesktops (ewmh, fullscreenEventHook)
import XMonad.Hooks.ManageDocks (manageDocks, docks, avoidStruts)
import XMonad.Hooks.UrgencyHook (focusUrgent)
import XMonad.Hooks.ManageHelpers (isFullscreen, doFullFloat)

import XMonad.Util.EZConfig (mkKeymap)

import XMonad.Actions.FindEmptyWorkspace (viewEmptyWorkspace, tagToEmptyWorkspace)
import XMonad.Actions.GridSelect (goToSelected)
import XMonad.Actions.PhysicalScreens (viewScreen, sendToScreen)

import System.Taffybar.Hooks.PagerHints (pagerHints)

import qualified XMonad.StackSet as W

main :: IO ()
main = xmonad $
       docks $
       ewmh $
       pagerHints $ -- pass information to taffybar
       def {
             terminal           = "urxvtcd"
           , normalBorderColor  = "#cccccc"
           , focusedBorderColor = "#2342ff"
           , keys = keyMap
           , manageHook = composeAll [ manageHook def
                                     , manageDocks
                                     , isFullscreen --> doFullFloat
                                     ]
           , layoutHook = layout
           , handleEventHook = handleEventHook def <+> fullscreenEventHook
           , modMask = mod4Mask
           , workspaces = workSpaces
           , focusFollowsMouse = False
           , clickJustFocuses = False
           }

workSpaces :: [String]
workSpaces = map show ([1 .. 9] :: [Integer])

layout = smartBorders $ avoidStruts (tall ||| Mirror tall ||| Full ||| Grid ||| twoPane ||| twoCols)
  where twoPane = TwoPane (3/100) (1/2)
        tall    = Tall 1 (3/100) (1/2)
        twoCols = multiCol [1] 0 (3/100) (1/2)

keyMap :: XConfig l -> Map (KeyMask, KeySym) (X ())
keyMap c = mkKeymap c $
  [ ("M-S-<Return>" , spawn $ XMonad.terminal c)
  , ("M-c"          , kill)
  , ("M-<Space>"    , sendMessage NextLayout)
  , ("M-n"          , refresh)
  , ("M-p"          , spawn "dmenu_run")
  , ("M-S-x"        , spawn "xtrlock")
  , ("M-e"          , spawn "emacsclient -a \"\" -c")
  , ("M-k"          , windows W.focusUp)
  , ("M-j"          , windows W.focusDown)
  , ("M-m"          , viewEmptyWorkspace)
  , ("M-S-m"        , tagToEmptyWorkspace)
  , ("M-S-k"        , windows W.swapUp)
  , ("M-S-j"        , windows W.swapDown)
  , ("M-<Return>"   , windows W.swapMaster)
  , ("M-q"          , broadcastMessage ReleaseResources >> restart "xmonad" True)
  , ("M-S-q"        , io exitSuccess)
  , ("M-g"          , focusUrgent)
  , ("M-t"          , withFocused $ windows . W.sink)
  , ("M-S-g"        , goToSelected def)
  , ("M-h"          , sendMessage Shrink)
  , ("M-i"          , sendMessage Expand)
  , ("M-v"          , spawn "xwit -current -warp 10000 10000") -- move cursor away
  , ("M-,"          , viewScreen 0)
  , ("M-."          , viewScreen 1)
  , ("M-S-,"        , sendToScreen 0)
  , ("M-S-."        , sendToScreen 1)
  , ("<XF86ScreenSaver>" , spawn "xtrlock")
  ]
  ++
  [ layoutMap k l
    | (k, l) <- [ ("f", "Full")
                , ("g", "Grid")
                , ("d", "Tall")
                , ("2", "TwoPane")
                , ("c", "MultiCol")
                ]]
  ++
  [(m ++ k, windows $ f w)
    | (w, k) <- zip (XMonad.workspaces c) (map show ([1..9] :: [Integer])),
      (m, f) <- [("M-",W.greedyView), ("M-S-",W.shift)]]
  ++
  [ ("<XF86AudioPlay>", spawn "exec emacsclient -n -e '(emms-pause)'")
  , ("<XF86AudioNext>", spawn "exec emacsclient -n -e '(emms-next)'")
  , ("<XF86AudioPrev>", spawn "exec emacsclient -n -e '(emms-previous)'")
  , ("<XF86AudioStop>", spawn "exec emacsclient -n -e '(emms-stop)'")
  ]
  ++
  [ ("M-a " ++ k, spawn $ "exec emacsclient -ne '(db/emacsclient-key \"" ++ k ++ "\")'")
    | k <- ["w", "o", "b", "h", "p", "c", "1", "2", "l", "d"] ]
  where
    layoutMap k l = ("M-l M-" ++ k, sendMessage $ JumpToLayout (l :: String))
