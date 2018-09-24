{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import           GHC.Generics (Generic)
import qualified Data.Maybe as Maybe
import qualified Control.Exception as Exception

-- Core
import qualified System.Envy as Envy
import qualified Network.Wai.Handler.Warp as Warp
import qualified Network.Wai as Wai
import qualified Network.HTTP.Types.Status as Status
import qualified Network.HTTP.Types.Header as Header
import qualified Network.HTTP.Client as HTTP
import qualified Network.HTTP.Client.TLS as TLS

-- Validation
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Lazy.Char8 as LBSC
import qualified Data.CaseInsensitive as CI
import qualified Data.Validation as Validation

-- | Runtime settings to control how we run
newtype Config
  = Config
  { diffPort :: Int -- "DIFF_PORT"
  } deriving (Generic, Show)
instance Envy.DefConfig Config where
  defConfig = Config 3000
instance Envy.FromEnv Config

-- | Possible errors we may emit
data Error
  = MisConfiguredEnv String
  | HeaderNotFound Header.HeaderName
  | InvalidHost Header.HeaderName Exception.SomeException
  -- | ResponseMismatch Diff
instance Show Error where
  show (MisConfiguredEnv msg)
    = "Unable to parse ENV configs: " ++ msg
  show (HeaderNotFound headerName)
    = "Required header not found: "
    ++ showHeader headerName
  show (InvalidHost headerName err)
    = "Invalid host given in header ("
    ++ showHeader headerName
    ++ "): "
    ++ show err
  -- show (ResponseMismatch diff)
  --   = "Hosts have yielded different responses: "
  --   ++ show diff

-- | Entry point
main :: IO ()
main = do
  eConfig <- Envy.decodeEnv
  case eConfig of
    (Left err) -> logError $ MisConfiguredEnv err
    (Right config) -> do
      let port = diffPort config
      let preRunHook = logInfo $ "Listening on port: " ++ show port
      let settings
            = Warp.setPort port
            . Warp.setBeforeMainLoop preRunHook
            $ Warp.defaultSettings
      manager <- TLS.newTlsManager
      Warp.runSettings settings $ mkApp manager

-- | The app
mkApp :: HTTP.Manager -> Wai.Application
mkApp manager req respond
  = respond
  =<< Validation.validation
      (pure . mkErrorResponse)
      (uncurry go)
      (Validation.bindValidation getHeaders mkRequests)
  where
    headers = Wai.requestHeaders req
    mkErrorResponse :: [Error] -> Wai.Response
    mkErrorResponse = Wai.responseLBS Status.badRequest400 [] . LBSC.pack . show
    getHeaders
      = (,)
      <$> getHeader headers "DIFF_HOST_A"
      <*> getHeader headers "DIFF_HOST_B"
    mkRequests (h1,h2)
      = (,)
      <$> mkHTTPRequest h1
      <*> mkHTTPRequest h2
    go :: HTTP.Request -> HTTP.Request -> IO Wai.Response
    go reqA _reqB = do
      let reqA' = copyWaiToHTTP req reqA
      response <- HTTP.httpLbs reqA' manager
      return . httpToWaiResponse $ response

--------------------------------------------------------------------------------
-- * Ferrying Request/Response types between WAI and HTTP-Client

-- | Construct an HTTP request from an incoming header.
mkHTTPRequest :: Header.Header -> Validation.Validation [Error] HTTP.Request
mkHTTPRequest (headerName, host)
  = fmap HTTP.setRequestIgnoreStatus
  . Validation.liftError
    (pure . InvalidHost headerName)
  . HTTP.parseUrlThrow
  . BSC.unpack
  $ host

-- | Transfer request data from Wai to HTTP
copyWaiToHTTP :: Wai.Request -> HTTP.Request -> HTTP.Request
copyWaiToHTTP wai http
  = http
  { HTTP.path = Wai.rawPathInfo wai
  , HTTP.queryString = Wai.rawQueryString wai
  , HTTP.requestHeaders = sanitizeHeaders $ Wai.requestHeaders wai
  , HTTP.requestBody = waiToHTTPBody wai
  }

-- | Transform WAI request body into HTTP
waiToHTTPBody :: Wai.Request -> HTTP.RequestBody
waiToHTTPBody wai = go (Wai.requestBody wai) (Wai.requestBodyLength wai)
  where
    go b Wai.ChunkedBody = HTTP.RequestBodyStreamChunked (\k -> k b)
    go b (Wai.KnownLength l) = HTTP.RequestBodyStream (fromIntegral l) (\k -> k b)

-- | Transform HTTP response to WAI
httpToWaiResponse :: HTTP.Response LBSC.ByteString -> Wai.Response
httpToWaiResponse http
  = Wai.responseLBS
  (HTTP.responseStatus http)
  (sanitizeHeaders $ HTTP.responseHeaders http)
  (HTTP.responseBody http)

--------------------------------------------------------------------------------
-- * Utilities

-- | Remove unsafe headers. See 'isUnsafeHeader'
sanitizeHeaders :: [Header.Header] -> [Header.Header]
sanitizeHeaders = filter (not . isUnsafeHeader . fst)

-- | Unsafe according to HTTP Client
-- https://hackage.haskell.org/package/http-client-0.5.13.1/docs/Network-HTTP-Client.html#v:responseOpen
isUnsafeHeader :: Header.HeaderName -> Bool
isUnsafeHeader a = elem a
  [ "Transfer-Encoding"
  , "Content-Length"
  , "Content-Encoding"
  , "Accept-Encoding"
  ]

-- | Extract a header by name from a bag
getHeader
  :: Header.RequestHeaders -> Header.HeaderName
  -> Validation.Validation [Error] Header.Header
getHeader headers h = Maybe.maybe
  (Validation.Failure . pure . HeaderNotFound $ h)
  (\v -> pure (h,v))
  $ lookup h headers

-- | Convert header to string
showHeader :: Header.HeaderName -> String
showHeader = BSC.unpack . CI.original

-- | Log info
logInfo :: String -> IO ()
logInfo str = putStrLn $ "[INFO] " ++ str

-- | Log error
logError :: Error -> IO ()
logError err = putStrLn $ "[ERROR] " ++ show err
