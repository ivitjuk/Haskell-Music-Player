{-# LANGUAGE FlexibleContexts, GeneralizedNewtypeDeriving #-}

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

module HMP (

             HMP,

             execHMP,

             getMpd

            ) where
    
import Control.Monad
import Control.Monad.Reader
import Control.Monad.State

import Network.MPD

newtype HMP a = HMP
    { 
      runHMP :: (StateT HMPState (ReaderT HMPConfig IO)) a
    } deriving (Functor, Monad, MonadIO)

data HMPState = HMPState 
    {
      dummy :: String -- this is for future use
    }

data HMPConfig = HMPConfig 
    {
      mpd :: MPDPersistent
    }

execHMP :: Host -> Port -> Password -> HMP () -> IO ()
execHMP host port pw action = do
  Right mpd <- makeMPDPersistent host port pw
  runReaderT (evalStateT (runHMP action) (HMPState "")) (HMPConfig mpd)
  return ()

getMpd :: HMP MPDPersistent
getMpd = HMP $ do
  cfg <- ask
  return (mpd cfg)
