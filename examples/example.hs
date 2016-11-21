{-

This file is part of the Haskell package playlists-http. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists-http/LICENSE. No part
of playlists-http package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
module Main (main) where

--------------------------------------------------------------------------------
import Text.Playlist.HTTP.Full

--------------------------------------------------------------------------------
import Control.Monad (when)
import qualified Data.Text as Text
import Network.HTTP.Client (newManager, defaultManagerSettings, Manager)
import System.Environment (getArgs)

--------------------------------------------------------------------------------
-- | Main!
main :: IO ()
main = do
    args <- getArgs
    when (null args) $ fail "Usage: example URL"

    -- For a TLS connection, use the @http-client-tls@ package!
    manager <- newManager defaultManagerSettings
    result  <- download (env manager) (Text.pack $ head args)

    case result of
      Left err -> fail (show err)
      Right pl -> print pl

  where
    -- | Create an @Environment@.
    env :: Manager -> Environment
    env m = Environment m limit

    -- | No download limit.
    limit :: Int -> ByteStatus
    limit _ = Continue
