{-

This file is part of the Haskell package playlists-http. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists-http/LICENSE. No part
of playlists-http package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | A simple interface for recursively downloading a 'Playlist'.
module Text.Playlist.HTTP.Simple
  ( download
  , module Playlist
  ) where

--------------------------------------------------------------------------------
-- Package imports:
import Data.Text (Text)
import Network.HTTP.Client
import Text.Playlist as Playlist

--------------------------------------------------------------------------------
-- Local imports:
import Text.Playlist.HTTP.Full (Environment(..), ByteStatus(..))
import qualified Text.Playlist.HTTP.Full as Full

--------------------------------------------------------------------------------
-- | Download the playlist whose URL is given in the first argument.
-- If the downloaded playlist references other playlists then it will
-- be recursively processed/downloaded.
--
-- This function will not download more than 5MB total.
--
-- This function does not support TLS/SSL.
--
-- For more control over the download limit, for using TLS/SSL, and
-- for proper error reporting, use the @download@ function from
-- 'Text.Playlist.HTTP.Full'.
download :: Text -> IO (Maybe Playlist)
download url = do
    manager <- newManager defaultManagerSettings
    either (const Nothing) Just <$> Full.download (env manager) url
  where
    env :: Manager -> Environment
    env m = Environment m under5MB

    under5MB :: Int -> ByteStatus
    under5MB n = if n < 5242880 then Continue else LimitReached
