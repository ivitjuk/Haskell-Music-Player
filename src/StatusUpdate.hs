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

module StatusUpdate
    (
     setup
     ) where

import Data.IORef
import Control.Monad.Trans (liftIO)

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import qualified Network.MPD as MPD
import qualified Network.MPD.Core as MPDC

import qualified Playlist as PL
import qualified Progressbar as PB

import qualified GuiData as GD

getMpdStatus' (Left e) = return MPD.defaultStatus
getMpdStatus' (Right s) = return s

getMpdStatus :: MPDC.MPDPersistent -> IO MPD.Status
getMpdStatus mpd = MPDC.withMPDPersistent mpd MPD.status >>= getMpdStatus'

statusUpdateCallback :: GD.GuiDataRef -> (GD.GuiDataRef  -> IO ()) -> IO Bool
statusUpdateCallback gdref pbCalback = do
  gd <- readIORef gdref
  status <- getMpdStatus (GD.mpd gd)
  writeIORef gdref (GD.GData 
                          (GD.mpd gd) 
                          (GD.pbar gd) 
                          (GD.playButton gd) 
                          (GD.currentStatus gd) 
                          status 
                          (GD.plist gd))
  pbCalback gdref
  PB.update gdref
  PL.update gdref
  return True

setup :: GD.GuiDataRef -> GladeXML -> (GD.GuiDataRef  -> IO ()) -> IO ()
setup gdref xml pbCallback = do
  liftIO $ timeoutAdd (statusUpdateCallback gdref pbCallback) 1000
  return ()
