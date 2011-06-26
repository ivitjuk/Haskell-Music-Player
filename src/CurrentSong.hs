{-# LANGUAGE  ScopedTypeVariables #-}
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

module CurrentSong
    (
     Data,
     CurrentSong.init,
     update,
    ) where

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade
import Graphics.UI.Gtk.Gdk.Events

import Data.IORef
import Data.Maybe (fromMaybe)

import qualified Network.MPD as MPD
import qualified Network.MPD.Core as MPDC

import qualified Util as U
import qualified GuiData as GD 

data Data = CSData 
    {
      theSong :: Label
    }

update :: GD.GuiDataRef -> IO ()
update gdref = do 
  gd <- readIORef gdref
  MPDC.withMPDPersistent (GD.mpd gd) MPD.currentSong >>= 
                          (\s -> case s of (Left e) -> return ()
                                           (Right s) -> labelSetMarkup (theSong (GD.currentSong gd)) 
                                                        ("<b>" ++ (U.showSongTag s MPD.Title) ++ 
                                                         "</b>\n <small>by " ++ (U.showSongTag s MPD.Artist) ++ 
                                                         " from " ++ (U.showSongTag s MPD.Album) ++ "</small>"
                                                        ))
  return ()

init :: GladeXML -> IO Data
init xml = do
  cs <- xmlGetWidget xml castToLabel "currentSong"
  labelSetSingleLineMode cs False
  expander <- xmlGetWidget xml castToExpander "expander"
  mw <- xmlGetWidget xml castToWindow "mainWindow"
  afterActivate expander $ do expanderGetExpanded expander >>= windowSetResizable mw 
                              windowResize mw 450 500
  return $ CSData cs
