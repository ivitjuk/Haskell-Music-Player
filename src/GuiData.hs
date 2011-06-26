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

module GuiData
    (
     GuiData(GData),
     GuiDataRef,
     mpd, pbar, playButton, prevStatus, currentStatus, plist, GuiData.currentSong
    ) where

import Network.MPD
import Graphics.UI.Gtk

import Data.IORef

import {-# SOURCE #-} qualified Progressbar as PB
import {-# SOURCE #-} qualified Playlist as PL
import {-# SOURCE #-} qualified CurrentSong as CS

data GuiData = GData
    {
      mpd :: MPDPersistent,
             
      playButton :: Button,
                    
      prevStatus :: Network.MPD.Status,
      currentStatus :: Network.MPD.Status,
                       
      pbar :: PB.Data,
      plist :: PL.Data,
      currentSong :: CS.Data
    }

type GuiDataRef = IORef GuiData
