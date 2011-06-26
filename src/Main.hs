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

import qualified Network.MPD as MPD
import qualified Network.MPD.Core as MPDC

import Data.IORef
import Control.Monad.Trans (liftIO)
import Data.Maybe (fromMaybe)
import Data.Either (rights)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import qualified Playlist as PL
import qualified Progressbar as PB
import qualified StatusUpdate as SU
import qualified CurrentSong as CS
import qualified GuiData as GD
import qualified Util as U
import qualified HMP as HMP

execMpdCommand gdref cmd = do
  gd <- readIORef gdref
  MPDC.withMPDPersistent (GD.mpd gd) cmd

setupPlayButton' btn MPD.Paused = buttonSetLabel btn "Play"
setupPlayButton' btn MPD.Stopped = buttonSetLabel btn "Play"
setupPlayButton' btn MPD.Playing = buttonSetLabel btn "Pause"

setupPlayButton :: GD.GuiDataRef  -> IO ()
setupPlayButton gdref = do
  gd <- readIORef gdref
  setupPlayButton' (GD.playButton gd) (MPD.stState (GD.currentStatus gd))

processPlayButton' gdref MPD.Paused = execMpdCommand gdref (MPD.pause False)
processPlayButton' gdref MPD.Stopped = execMpdCommand gdref (MPD.play Nothing)
processPlayButton' gdref MPD.Playing = execMpdCommand gdref (MPD.pause True)

processPlayButton :: GD.GuiDataRef -> Button -> IO ()
processPlayButton gdref btn = do
  gd <- readIORef gdref
  processPlayButton' gdref (MPD.stState (GD.currentStatus gd))
  return ()

setupUi :: GD.GuiDataRef -> GladeXML -> HMP.HMP ()
setupUi gdref xml = do
  gd <- liftIO $ readIORef gdref

  -- main window
  window   <- liftIO $ xmlGetWidget xml castToWindow "mainWindow"
  liftIO $ onDestroy window mainQuit

  -- buttons
  prevButton <- liftIO $ xmlGetWidget xml castToButton "prevButton"
  liftIO $ onClicked prevButton $ do execMpdCommand gdref MPD.previous
                                     return ()

  nextButton <- liftIO $ xmlGetWidget xml castToButton "nextButton"
  liftIO $ onClicked nextButton $ do execMpdCommand gdref MPD.next
                                     return ()

  playButton <- liftIO $ xmlGetWidget xml castToButton "playButton"
  liftIO $ onClicked playButton $ do processPlayButton gdref playButton

  stopButton <- liftIO $ xmlGetWidget xml castToButton "stopButton"
  liftIO $ onClicked stopButton $ do execMpdCommand gdref MPD.stop
                                     return ()

  liftIO $ PB.setup gdref

  liftIO $ PL.setup gdref xml

  liftIO $ SU.setup gdref xml setupPlayButton

  -- show
  liftIO $ widgetShowAll window

  return ()

makeGuiRef :: GladeXML -> HMP.HMP GD.GuiDataRef
makeGuiRef xml = do
  mpd <- HMP.getMpd

  playButton <- liftIO $ xmlGetWidget xml castToButton "playButton"

  plist <- liftIO $ PL.init xml

  pbar <- liftIO $ PB.init xml

  csong <- liftIO $ CS.init xml

  gdref <- liftIO $ newIORef $ 
           GD.GData 
           mpd 
           playButton 
           MPD.defaultStatus 
           MPD.defaultStatus 
           pbar
           plist
           csong

  return gdref

gui :: HMP.HMP ()
gui = do
  liftIO initGUI
  Just xml <- liftIO $ xmlNew "HaskellMusicPlayer.glade"
  gdref <- makeGuiRef xml
  setupUi gdref xml
  liftIO mainGUI
  return ()

main :: IO ()
main = do
  HMP.execHMP "localhost" 6600 "" gui
