module ElmBuilder.HttpError (Error(..)) where

import Control.Exception (SomeException)
import Network.HTTP.Client (HttpExceptionContent)

data Error
  = BadUrl String String
  | BadHttp String HttpExceptionContent
  | BadMystery String SomeException
