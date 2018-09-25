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
import qualified Data.Aeson as Aeson
import           Data.Aeson ((.=))
import qualified Control.Concurrent.Async as Async

-- Diff
import qualified System.Process as Process
import qualified Text.Diff.Parse as Diff
import qualified Text.Diff.Parse.Types as Diff
import qualified System.IO.Temp as Temp
import qualified System.Exit as Exit
import qualified Data.Text as T

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
  | UnexpectedDiffFailure Int
  | UnexpectedDiffParseError String
  | ResponseMismatch [Diff]
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
  show (UnexpectedDiffFailure err)
    = "Diff command failed with code: "
    ++ show err
  show (UnexpectedDiffParseError err)
    = "Unable to parse diff: "
    ++ err
  show (ResponseMismatch diff)
    = "Hosts have yielded different responses: "
    ++ show diff
instance Aeson.ToJSON Error where
  toJSON (ResponseMismatch diff)
    = Aeson.object
    [ "message" .= ("Hosts have yielded different responses" :: String)
    , "diff" .= diff
    ]
  toJSON x = Aeson.toJSON . show $ x

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
    headersJson = [("Content-Type", "application/json")]
    mkErrorResponse :: [Error] -> Wai.Response
    mkErrorResponse = Wai.responseLBS Status.badRequest400 headersJson . Aeson.encode
    getHeaders
      = (,)
      <$> getHeader headers "DIFF_HOST_A"
      <*> getHeader headers "DIFF_HOST_B"
    mkRequests (h1,h2)
      = (,)
      <$> mkHTTPRequest h1
      <*> mkHTTPRequest h2
    go :: HTTP.Request -> HTTP.Request -> IO Wai.Response
    go reqA reqB = do
      let runReq r = HTTP.httpLbs (copyWaiToHTTP req r) manager
      (resA, resB) <- Async.concurrently (runReq reqA) (runReq reqB)
      Validation.validation mkErrorResponse id <$> compareResponses resA resB

--------------------------------------------------------------------------------
-- * Diff

compareResponses
  :: HTTP.Response LBSC.ByteString -> HTTP.Response LBSC.ByteString
  -> IO (Validation.Validation [Error] Wai.Response)
compareResponses resA resB = do
  let resBodyA = HTTP.responseBody resA
  let resBodyB = HTTP.responseBody resB
  let ok = pure . httpToWaiResponse $ resB
  if resBodyA == resBodyB
    then pure ok
    else do
      eDiff <- calculateDiff resBodyA resBodyB
      pure $ case eDiff of
        (Validation.Success []) -> ok
        (Validation.Success ds) -> Validation.Failure . pure . ResponseMismatch $ ds
        (Validation.Failure f) -> Validation.Failure f

calculateDiff
  :: LBSC.ByteString -> LBSC.ByteString
  -> IO (Validation.Validation [Error] [Diff])
calculateDiff a b = do
  tmpDir <- Temp.getCanonicalTemporaryDirectory
  Temp.withTempFile tmpDir "left" $ \fpA hA ->
    Temp.withTempFile tmpDir "right" $ \fpB hB -> do
      LBSC.hPut hA a
      LBSC.hPut hB b
      (x, diff, e) <- Process.readProcessWithExitCode "diff" ["-u", fpA, fpB] ""
      case x of
        Exit.ExitSuccess -> pure . pure $ []
        (Exit.ExitFailure 1) -> pure . pure $ parseDiff diff
        (Exit.ExitFailure n) -> do
          logInfo . show $ e
          pure . Validation.Failure . pure . UnexpectedDiffFailure $ n

parseDiff :: String -> [Diff]
parseDiff str = Validation.validation (const . pure . DiffRaw $ str) id $ parseDiff' str

parseDiff' :: String -> Validation.Validation [Error] [Diff]
parseDiff' str
  = fmap (fmap DiffParsed)
  $ Validation.liftError (pure . UnexpectedDiffParseError)
  $ Diff.parseDiff (T.pack str)

data Diff
  = DiffParsed Diff.FileDelta
  | DiffRaw String
  deriving Show
instance Aeson.ToJSON Diff where
  toJSON (DiffParsed fd)
    = Aeson.object
    [ "status" .= show (Diff.fileDeltaStatus fd)
    , "sourceFile" .= Diff.fileDeltaSourceFile fd
    , "destFile" .= Diff.fileDeltaDestFile fd
    , "content" .= contentToJSON (Diff.fileDeltaContent fd)
    ]
  toJSON (DiffRaw s) = Aeson.toJSON s

contentToJSON :: Diff.Content -> Aeson.Value
contentToJSON c@Diff.Binary = Aeson.toJSON $ show c
contentToJSON (Diff.Hunks hs) = Aeson.toJSONList $ hunkToJSON <$> hs

hunkToJSON :: Diff.Hunk -> Aeson.Value
hunkToJSON h
  = Aeson.object
  [ "sourceRange" .= rangeToJSON (Diff.hunkSourceRange h)
  , "destRange" .= rangeToJSON (Diff.hunkDestRange h)
  , "hunkLines" .= (lineToJSON <$> Diff.hunkLines h)
  ]

rangeToJSON :: Diff.Range -> Aeson.Value
rangeToJSON r
  = Aeson.object
  [ "startingLineNumber" .= Diff.rangeStartingLineNumber r
  , "numberOfLines" .= Diff.rangeNumberOfLines r
  ]

lineToJSON :: Diff.Line -> Aeson.Value
lineToJSON l
  = Aeson.object
  [ "annotation" .= show (Diff.lineAnnotation l)
  , "content" .= Diff.lineContent l
  ]

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
