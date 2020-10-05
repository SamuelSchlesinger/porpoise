{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE RankNTypes #-}
{- |
Module: Porpoise
Description: A very minimal web framework wrapping wai
Copyright: (c) Samuel Schlesinger 2020
License: MIT
Maintainer: sgschlesinger@gmail.com
Stability: experimental
Portability: POSIX, Windows

A very minimal HTTP server framework wrapping wai.
-}
module Web.Porpoise
( 
  -- ** Server Language
  Server(..)
, toApplication
, liftS
, serverIO
, Profunctor(..)
, Category(..)
, MonadUnliftIO(..)
, ResponseReceived
, Application
  -- ** Observing a 'Request' 
, Request
, requestMethod
, httpVersion
, rawPathInfo
, rawQueryString
, requestHeaders
, isSecure
, remoteHost
, pathInfo
, queryString
, getRequestBodyChunk
, vault
, requestBodyLength
, requestHeaderHost
, requestHeaderRange
, requestHeaderReferer
, requestHeaderUserAgent
, strictRequestBody
, lazyRequestBody
, RequestBodyLength(..)
  -- ** Building a 'Response'
, Response
, FilePart(..)
, responseLBS
, responseStream
, responseRaw
, responseBuilder
, responseFile
, StreamingBody
  -- ** Miscellaneous re-exports
, Status
, Header
, HeaderName
, ResponseHeaders
, RequestHeaders
, hAccept
, hAcceptCharset
, hAcceptEncoding
, hAcceptLanguage
, hAcceptRanges
, hAge
, hAllow
, hAuthorization
, hCacheControl
, hConnection
, hContentEncoding
, hContentLanguage
, hContentLocation
, hContentMD5
, hContentRange
, hContentType
, hDate
, hETag
, hExpect
, hExpires
, hFrom
, hHost
, hIfMatch
, hIfModifiedSince
, hIfNoneMatch
, hIfRange
, hIfUnmodifiedSince
, hLastModified
, hLocation
, hMaxForwards
, hOrigin
, hPragma
, hPrefer
, hPreferenceApplied
, hProxyAuthenticate
, hRange
, hReferer
, hRetryAfter
, hServer
, hTE
, hTrailer
, hTransferEncoding
, hUpgrade
, hUserAgent
, hVary
, hVia
, hWWWAuthenticate
, hWarning
, hContentDisposition
, hMIMEVersion
, hCookie
, hSetCookie
, ByteRange(..)
, renderByteRangeBuilder
, renderByteRange
, ByteRanges
, renderByteRangesBuilder
, parseByteRanges
, HttpVersion(..)
, http09
, http10
, http11
, http20
, Method
, methodGet
, methodPost
, methodHead
, methodPut
, methodDelete
, methodTrace
, methodConnect
, methodOptions
, methodPatch
, StdMethod(..)
, parseMethod
, renderMethod
, renderStdMethod
, QueryItem
, Query
, urlEncode
, urlDecode
, urlEncodeBuilder
, extractPath
, decodePath
, encodePath
, SockAddr
, Vault
) where

import Prelude hiding ((.), id)
import Data.Coerce (coerce)
import Control.Arrow (Kleisli(Kleisli))
import Data.Profunctor (Profunctor(..))
import Control.Monad.Fail (MonadFail)
import Control.Monad.Reader (MonadReader, ReaderT(ReaderT))
import Control.Monad.Cont (MonadCont, ContT(ContT))
import Control.Category (Category((.), id))
import Network.Wai
import Network.Wai.Internal (ResponseReceived(ResponseReceived))
import UnliftIO (MonadIO(liftIO), MonadUnliftIO(withRunInIO))
import Network.HTTP.Types.Header
import Network.HTTP.Types.Version
import Network.HTTP.Types.Method
import Network.HTTP.Types.URI
import Network.HTTP.Types.Status
import Data.Vault.Lazy (Vault)
import Network.Socket (SockAddr)

{- |
A server application which receives a request and responds.
-}
newtype Server m request response = Server
  { unServer :: request -> (response -> m ResponseReceived) -> m ResponseReceived }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFail, MonadReader request, MonadCont)
    via ReaderT request (ContT ResponseReceived m)

instance Functor m => Profunctor (Server m) where
  dimap f g (Server h) = Server \request respond -> h (f request) (respond . g)

instance Monad m => Category (Server m) where
  Server a . Server b = coerce $ Kleisli (fmap ContT a) . Kleisli (fmap ContT b)
  id :: forall a. Server m a a
  id = Server \request respond -> respond request

{- |
Compile a 'Server' a runnable wai application.
-}
{-# INLINE toApplication #-}
toApplication :: Server IO Request Response -> Application
toApplication = unServer

{- |
Lift a computation from the base monad into the 'Server' monad. This is
provided because this library prefers to use the second to last type variable
position for the contravariant component in the 'Profunctor' instance,
and so we are able to write a 'Category' instance.
-}
{-# INLINE liftS #-}
liftS :: Monad m => m response -> Server m request response
liftS r = Server $ const (r >>=)

{- |
In any monad that has an instance of 'MonadUnliftIO', we can retrieve a
function allowing our 'Server' to operate in 'IO'. The resulting function is
expected to be used to transform the 'Server' prior to calling 'toApplication'.
-}
{-# INLINE serverIO #-}
serverIO :: MonadUnliftIO m => m (Server m request response -> Server IO request response)
serverIO =
  withRunInIO
    \runner -> pure
      \(Server f) -> Server
        \request respond -> runner (f request (liftIO . respond))
