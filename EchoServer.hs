-- Example of echo server

module EchoServer where

import Control.Applicative ((<$>))
import Control.Concurrent
import Control.Exception
import Network.Socket
import System.IO

echoServer :: ServiceName -> IO ()
echoServer port = withSocketsDo $ do
  serverAddr <- getaddrinfo port
  bracket (listenOn serverAddr) close $ \sock -> do
    lock <- newMVar ()
    procRequests lock sock

getaddrinfo :: ServiceName -> IO AddrInfo
getaddrinfo port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE]
                                        -- , addrFamily = AF_INET
                                           , addrSocketType = Stream }))
                       Nothing (Just port)

listenOn :: AddrInfo -> IO Socket
listenOn serverAddr = do
  sock <- socket (addrFamily serverAddr)
                 (addrSocketType serverAddr)
                 defaultProtocol
  bindSocket sock (addrAddress serverAddr)
  listen sock 5
  return sock

procRequests :: MVar () -> Socket -> IO ()
procRequests lock masterSock = do
  (connSock, clientAddr) <- accept masterSock
  _ <- forkIO $ procMessages lock connSock clientAddr
  procRequests lock masterSock

procMessages :: MVar () -> Socket -> SockAddr -> IO ()
procMessages lock connSock clientAddr = do
  connhdl <- socketToHandle connSock ReadWriteMode
  hSetBuffering connhdl LineBuffering
  handleMessage lock connhdl clientAddr "echoServer: client connected."
  messages <- hGetContents connhdl
  mapM_ (handleMessage lock connhdl clientAddr) (lines messages)
  handleMessage lock connhdl clientAddr "echoServer: client disconnected."
  hClose connhdl

handleMessage :: MVar () -> Handle -> SockAddr -> String -> IO ()
handleMessage lock connhdl clientAddr msg = do
  hPutStrLn connhdl msg
  withMVar lock (\a -> handlerFunc clientAddr msg >> return a)

handlerFunc :: SockAddr -> String -> IO ()
handlerFunc addr msg = do
  putStrLn $ "From " ++ show addr ++ ": " ++ msg
