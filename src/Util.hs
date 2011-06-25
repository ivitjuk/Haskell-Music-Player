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

module Util (

             prettyTime,
             
             showTag, showMaybe

            ) where

import Network.MPD
    
import Data.Map
import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Format
import System.Locale

prettyTime sec =
    formatTime defaultTimeLocale "%T" t 
        where t = UTCTime (ModifiedJulianDay 0) (secondsToDiffTime sec)

showTag :: Map Metadata [String] -> Metadata -> String
showTag tags tag = head $ findWithDefault ["N/A"] tag tags

showMaybe :: (Show a) => Maybe a -> String
showMaybe (Just a) = show a
showMaybe _ = ""
