module Bootstrap.Server.Main where 

import Debug.Trace
import Data.Function (Fn3(..))
import Data.Foreign.EasyFFI
import Control.Monad.Eff
import Control.Monad.Eff.Class
import Control.Monad.Eff.Exception
import Node.Express.Types
import Node.Express.App
import Node.Express.Handler
import qualified Node.FS.Async as File
import qualified Node.Buffer as Buf
import Node.Encoding

foreign import staticMiddleware "var staticMiddleware = require('express').static"
    :: String -> Fn3 Request Response (ExpressM Unit) (ExpressM Unit)

logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace url
    next
    
errorHandler :: Error -> Handler
errorHandler err = do
    setStatus 400
    sendJson {error: message err}
            
appSetup :: App
appSetup = do
    use logger
    useExternal $ staticMiddleware "public"
    useOnError errorHandler
    
main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listenHttp appSetup port \_ ->
        trace $ "Server running on localhost:" ++ show port
