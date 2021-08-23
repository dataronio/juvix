{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}

module Juvix.HTTP.API
  ( API,
    apiProxy,
    api,
    runServer,
  )
where

import Data.Aeson.Types ()
import qualified Juvix.HTTP.Routes as Routes
import Juvix.Library
import Network.Wai.Handler.Warp
import Network.Wai.Middleware.Cors
import Network.Wai.Middleware.RequestLogger (logStdoutDev)
import Servant

type API = Routes.Pipeline

apiProxy :: Proxy API
apiProxy = Proxy

api :: Server API
api = Routes.pipeline

runServer :: IO ()
runServer = do
  let port = 3001
  putText $ "Server is running on port " <> show port
  run port . logStdoutDev . customCors . serve apiProxy $ api
  where
    customCors = cors (const $ Just (simpleCorsResourcePolicy {corsRequestHeaders = ["Accept", "Accept-Language", "Content-Language", "Content-Type"]}))
