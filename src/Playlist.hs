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

module Playlist
    (
     Data,
     Playlist.init, setup,
     update, process
     ) where

import qualified Network.MPD.Core as MPDC

import Graphics.UI.Gtk
import Graphics.UI.Gtk.Glade

import Data.IORef
import Data.Maybe (fromMaybe)
import Control.Monad.Trans (liftIO)
import qualified Network.MPD as MPD

import qualified Util as U
import qualified GuiData as GD 

data ListStoreEntry = ListStoreEntry 
    {
      songIsCurrent :: Bool,
      songArtist :: String,
      songName :: String,
      songLength :: String
    }

type ListStoreType = ListStore ListStoreEntry

data Data = PLData 
    {
      model :: ListStoreType
    }

process' :: Data -> Int -> Int -> [MPD.Song] -> IO ()
process' st songIdx currentSong [] = return ()
process' st songIdx currentSong (x:xs) = do
  let tags = MPD.sgTags x
  listStoreAppend  (model st) $ ListStoreEntry 
                       (songIdx == currentSong)
                       (U.showTag tags MPD.Artist)
                       (U.showTag tags MPD.Title)
                       (U.prettyTime $ MPD.sgLength x)
  process' st (songIdx+1) currentSong xs

process :: Data -> Int -> MPD.Response [MPD.Song] -> IO ()
process st currentSong (Left _) = listStoreClear (model st)
process st currentSong (Right songs) = do
  listStoreClear (model st)
  process' st 0 currentSong songs

playListChanged :: MPD.Status -> MPD.Status -> Bool
playListChanged cur prev = (MPD.stPlaylistVersion cur) > (MPD.stPlaylistVersion prev) ||
                           (MPD.stSongPos cur) /= (MPD.stSongPos prev)

update :: GD.GuiDataRef -> IO ()
update gdref = do
  gd <- readIORef gdref
  let currentSong = fromMaybe 0 (MPD.stSongPos (GD.currentStatus gd))
  if playListChanged (GD.currentStatus gd) (GD.prevStatus gd)
   then MPDC.withMPDPersistent (GD.mpd gd) (MPD.playlistInfo Nothing) >>= 
        process (GD.plist gd) currentSong
   else return ()

init :: GladeXML -> IO Data
init xml = do
  model <- liftIO $ listStoreNew []
  view <- liftIO $ xmlGetWidget xml castToTreeView "currentPlaylist"

  liftIO $ treeViewSetModel view model
  liftIO $  treeViewSetHeadersVisible view True
  liftIO $ treeViewSetRulesHint view True

  artistCol <- liftIO $ treeViewColumnNew
  trackCol <- liftIO $ treeViewColumnNew
  timeCol <- liftIO $ treeViewColumnNew

  renderer <- liftIO $ cellRendererTextNew

  liftIO $ treeViewColumnSetTitle artistCol "Artist"
  liftIO $ treeViewColumnSetTitle trackCol "Track"
  liftIO $ treeViewColumnSetTitle timeCol "Length"

  liftIO $ treeViewColumnPackStart artistCol renderer True
  liftIO $ treeViewColumnPackStart trackCol renderer True
  liftIO $ treeViewColumnPackStart timeCol renderer True

  liftIO $ cellLayoutSetAttributes artistCol renderer model $ 
             \row -> 
             [ 
              cellText := songArtist row ,
              cellTextWeight := if (songIsCurrent row) then 1000 else 500
             ]
  liftIO $ cellLayoutSetAttributes trackCol renderer model $ \row -> [ cellText := songName row ]
  liftIO $ cellLayoutSetAttributes timeCol renderer model $ \row -> [ cellText := songLength row ]

  liftIO $ treeViewAppendColumn view artistCol
  liftIO $ treeViewAppendColumn view trackCol
  liftIO $ treeViewAppendColumn view timeCol

  return $ PLData model

setup :: GD.GuiDataRef -> GladeXML -> IO ()
setup gdref xml = do
  gd <- readIORef gdref
  view <- liftIO $ xmlGetWidget xml castToTreeView "currentPlaylist"
  liftIO $ onRowActivated view (\path col -> do MPDC.withMPDPersistent (GD.mpd gd) (MPD.play (Just (path !! 0)))
                                                return ())
  return ()
