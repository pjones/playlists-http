{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ScopedTypeVariables        #-}

{-

This file is part of the Haskell package playlists-http. It is subject to
the license terms in the LICENSE file found in the top-level directory
of this distribution and at git://pmade.com/playlists-http/LICENSE. No part
of playlists-http package, including this file, may be copied, modified,
propagated, or distributed except according to the terms contained in
the LICENSE file.

-}

--------------------------------------------------------------------------------
-- | A more complete interface for recursively downloading a
-- 'Playlist'.  For a simple interface for downloading playlists see
-- 'Text.Playlist.HTTP.Simple'.
module Text.Playlist.HTTP.Full
  ( download
  , Error (..)
  , Environment (..)
  , ByteStatus (..)
  , module Playlist
  ) where

--------------------------------------------------------------------------------
-- Package imports:
import Control.Monad.Catch
import Control.Monad.RWS.Strict
import Control.Monad.Trans.Either
import qualified Data.Attoparsec.ByteString as Atto
import Data.ByteString (ByteString)
import qualified Data.ByteString as ByteString
import Data.Text (Text)
import qualified Data.Text as Text
import Network.HTTP.Client
import Text.Playlist as Playlist

--------------------------------------------------------------------------------
-- | Possible error values produced while downloading and resolving a
-- 'Playlist'.
data Error = InvalidURL Text              -- ^ URL could not be parsed.
           | ResponseTooLarge             -- ^ Byte limit exceeded.
           | ProtocolError HttpException  -- ^ HTTP/Network error.
           | FailedToParse String         -- ^ Invalid playlist format.
           | FailedOnException String     -- ^ Unknown exception.
           deriving Show

--------------------------------------------------------------------------------
-- | A status flag used to indicate if a download byte limit has been reached.
data ByteStatus = Continue      -- ^ Continue processing network data.
                | LimitReached  -- ^ Abort playlist processing.

--------------------------------------------------------------------------------
-- | Details needed by the 'download' function to operate.
data Environment = Environment
  { -- | A 'Manager' object from the 'Network.HTTP.Client' library.
    -- If you want the download to take place via a TLS/SSL connection
    -- you need to create the 'Manager' object correctly using the
    -- @http-client-tls@ package.
    httpManager :: Manager

  , -- | A function that is used to limit the number of bytes that are
    -- downloaded while recursively processing playlists.
    --
    -- It is given the current number of bytes that have been
    -- downloaded/processed and should return a 'ByteStatus'.
    httpByteCheck :: Int -> ByteStatus
  }

--------------------------------------------------------------------------------
-- | Internal type used for keeping state.
data State = State
  { httpBytes :: Int
  }

--------------------------------------------------------------------------------
-- | Internal type used for managing state, access to the environment,
-- and access to IO.
newtype Download m a =
  Download { runDownload :: RWST Environment () State (EitherT Error m) a }

  deriving ( Functor
           , Applicative
           , Monad
           , MonadIO
           , MonadReader Environment
           , MonadState State
           )

--------------------------------------------------------------------------------
-- | 'MonadThrow' instance for 'Download'.
instance (Monad m) => MonadThrow (Download m) where
  throwM = Download . lift . left . FailedOnException . show

--------------------------------------------------------------------------------
-- | Internal helper function for getting a result out of a 'Download'
-- computation.  Returns the result and the final state.
runS :: (Monad m)
     => Download m a
     -> Environment
     -> State
     -> m (Either Error (a, State))
runS d e s = do
  result <- runEitherT $ runRWST (runDownload d) e s
  case result of
    Left err         -> return (Left err)
    Right (x, s', _) -> return (Right (x, s'))

--------------------------------------------------------------------------------
-- | Internal helper function for merging the state of one 'Download'
-- computation into another.  Mostly used for when the @m@ value below
-- is two different monads (e.g. generic @MonadIO@ and @IO@).
merge :: (Monad m) => Download m (a, State) -> Download m a
merge k = do
  (x, s) <- k
  put s
  return x

--------------------------------------------------------------------------------
-- | Given an 'Environment' and a URL, recursively download and
-- process a playlist.
--
-- For an example of using this function see the @example.hs@ file
-- included in this package.
download :: forall m. (MonadIO m)
         => Environment
         -> Text
         -> m (Either Error Playlist)
download env startURL = fmap fst <$> runS go env (State 0) where

  ------------------------------------------------------------------------------
  -- | Start playlist processing with the startURL.
  go :: Download m Playlist
  go = resolve [Track startURL Nothing] fetch

  ------------------------------------------------------------------------------
  -- | Turn a URL into a HTTP Request object.
  --
  -- FIXME: Need a MonadCatch here to catch an invalid URL and
  -- return the correct type of Error (InvalidURL).
  request :: Text -> Download m Request
  request = parseRequest . Text.unpack

  ------------------------------------------------------------------------------
  -- | Initiate a HTTP download in IO and then delegate the parsing of
  -- the response body to the parseBody function.
  fetch :: Text -> Download m Playlist
  fetch url = do
    r <- request url
    e <- ask
    s <- get

    -- Some voodoo to make a request in IO and process it in Download.
    merge $ safeIO $ withResponse r (httpManager e) $
      \body -> runS (parseBody url body) e s

  ------------------------------------------------------------------------------
  -- | Like liftIO but catch exceptions and turn them into an Error.
  safeIO :: IO (Either Error a) -> Download m a
  safeIO action = io (catch action stop)
    where
      io a = Download (lift (hoistEither =<< liftIO a))
      stop = return . Left . ProtocolError

--------------------------------------------------------------------------------
-- | Internal helper function to parse the body of a HTTP response.
-- This function is written with @MonadIO m@ but will actually be run
-- directly in @IO@ thanks to 'withResponse' :(
parseBody :: forall m. (MonadIO m)
          => Text
          -> Response BodyReader
          -> Download m Playlist
parseBody url response = do
    parser <- Download (lift (hoistEither lookupParser))
    bytes  <- readChunk
    dispatch (Atto.parse parser bytes)

  where

    ----------------------------------------------------------------------------
    -- | Figure out which parser we should be using.
    lookupParser :: Either Error (Atto.Parser Playlist)
    lookupParser =
      case parserForFormat <$> fileNameToFormat (Text.unpack url) of
        Nothing     -> Left (InvalidURL url)
        Just parser -> Right parser

    ----------------------------------------------------------------------------
    -- | Dispatch an attoparsec response.
    dispatch :: Atto.Result Playlist -> Download m Playlist
    dispatch (Atto.Fail _ _ err) = Download . lift $ left (FailedToParse err)
    dispatch (Atto.Partial f)    = readChunk >>= dispatch . f
    dispatch (Atto.Done _ r)     = return r

    ----------------------------------------------------------------------------
    -- | Read bytes from the HTTP body.
    readChunk :: Download m ByteString
    readChunk = do
      check <- asks httpByteCheck
      count <- gets httpBytes

      case check count of
        LimitReached -> Download . lift $ left ResponseTooLarge
        Continue     -> do
          bytes <- liftIO $ brRead (responseBody response)
          modify' (\s -> s {httpBytes = ByteString.length bytes + count})
          return bytes
