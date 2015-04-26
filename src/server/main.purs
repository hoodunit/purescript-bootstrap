module Main where 

import Debug.Trace
import Data.Either
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

logger :: Handler
logger = do
    url <- getOriginalUrl
    liftEff $ trace url
    next
    
errorHandler :: Error -> Handler
errorHandler err = do
    setStatus 400
    sendJson {error: message err}
       
indexHandler :: Handler
indexHandler = do
    cb <- capture sendContentsHandler
    liftEff $ File.readFile "public/index.html" cb
    
jsHandler :: String -> Handler
jsHandler file = do
    cb <- capture sendContentsHandler
    liftEff $ File.readFile file cb

sendContentsHandler :: Either Error Buf.Buffer -> Handler
sendContentsHandler eitherContents =
    case eitherContents of
        Left err ->
            send $ show err
        Right buffer ->
            send $ Buf.toString UTF8 buffer
            
appSetup :: App
appSetup = do
    setProp "json spaces" 4
    use logger
    get "/" indexHandler
    get "/public/js/client.js" $ jsHandler "public/js/client.js"
    useOnError errorHandler
    
main = do
    port <- unsafeForeignFunction [""] "process.env.PORT || 8080"
    listenHttp appSetup port \_ ->
        trace $ "Server running on localhost:" ++ show port
