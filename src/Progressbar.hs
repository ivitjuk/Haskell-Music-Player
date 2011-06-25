-- Haskell Music Player, client for the MPD (Music Player Daemon)
-- Copyright (C) 2011 Ivan Vitjuk <v@iv.id.au>

-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with this program.  If not, see <http://www.gnu.org/licenses/>.

module Progressbar
    (
     Data,
     Progressbar.init, setup,
     update,
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events

import Data.IORef
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)

import qualified Network.MPD as MPD
import qualified Network.MPD.Core as MPDC

import qualified Util as U
import qualified GuiData as GD 

data Data = PBData 
    {
      pbar :: ProgressBar
    }

update :: GD.GuiDataRef -> IO ()
update gdref = do
  gd <- readIORef gdref
  let (elapsed, total) = (MPD.stTime (GD.currentStatus gd))
  progressBarSetText (pbar (GD.pbar gd)) ((U.prettyTime (floor elapsed)) ++ " / " ++ (U.prettyTime total))
  if fromInteger total > 0.0 
   -- following single line causes cpu usage to rise from ~0.1% to ~3.0%
   -- strace shows a lot of read()/write(), probably to the X server
   then progressBarSetFraction (pbar (GD.pbar gd)) (elapsed / fromInteger total)
   else return ()

seekSong' :: GD.GuiDataRef -> Double -> Maybe MPD.Song -> IO ()
seekSong' gdref fraction (Nothing) = return ()
seekSong' gdref fraction (Just s) = do
  gd <- liftIO $ readIORef gdref
  let pos = round ((fromInteger (MPD.sgLength s)) * fraction)
  MPDC.withMPDPersistent (GD.mpd gd) (MPD.seek (fromMaybe 0 (MPD.stSongPos (GD.currentStatus gd))) (pos))
  return ()
         
seekSong :: GD.GuiDataRef -> Double -> IO ()
seekSong gdref fraction = do
  gd <- liftIO $ readIORef gdref
  MPDC.withMPDPersistent (GD.mpd gd) MPD.currentSong >>= 
                          (\s -> case s of (Left e) -> return ()
                                           (Right s) -> seekSong' gdref fraction s)

setupSongSeek :: GD.GuiDataRef -> IO (ConnectId ProgressBar)
setupSongSeek gdref = do
  gd <- liftIO $ readIORef gdref
  onButtonPress (pbar (GD.pbar gd)) $ (\(Button _ _ _ x y _ _ _ _) -> do
                                  (w, h) <- widgetGetSize (pbar (GD.pbar gd))
                                  let fraction = x / fromInteger (toInteger (w-1))
                                  progressBarSetFraction (pbar (GD.pbar gd)) fraction
                                  seekSong gdref fraction
                                  return True)

init :: GladeXML -> IO Data
init xml = do
  progress <- liftIO $ xmlGetWidget xml castToProgressBar "progress"
  return $ PBData progress

setup :: GD.GuiDataRef -> IO ()
setup gdref = do
  liftIO $ setupSongSeek gdref
  return ()
