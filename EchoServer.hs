module EchoServer where

import Network.Socket
import Control.Concurrent
import System.IO
import Control.Applicative ((<$>))

echoServer :: ServiceName -> IO ()
echoServer port = withSocketsDo $ do
  serverAddr <- getaddrinfo port
  sock <- socket (addrFamily serverAddr)
                 (addrSocketType serverAddr)
                 defaultProtocol
  bindSocket sock (addrAddress serverAddr)
  listen sock 5
  lock <- newMVar ()
  procRequests lock sock

getaddrinfo :: ServiceName -> IO AddrInfo
getaddrinfo port =
  head <$> getAddrInfo (Just (defaultHints { addrFlags = [AI_PASSIVE]
                                        -- , addrFamily = AF_INET
                                           , addrSocketType = Stream }))
                       Nothing (Just port)

procRequests :: MVar () -> Socket -> IO ()
procRequests lock masterSock = do
  (connSock, clientAddr) <- accept masterSock
  _ <- forkIO $ procMessages lock connSock clientAddr
  procRequests lock masterSock

procMessages :: MVar () -> Socket -> SockAddr -> IO ()
procMessages lock connSock clientAddr = do
  connhdl <- socketToHandle connSock ReadWriteMode
  hSetBuffering connhdl LineBuffering
  handle lock connhdl clientAddr "echoServer: client connected."
  messages <- hGetContents connhdl
  mapM_ (handle lock connhdl clientAddr) (lines messages)
  handle lock connhdl clientAddr "echoServer: client disconnected."
  hClose connhdl

handle :: MVar () -> Handle -> SockAddr -> String -> IO ()
handle lock connhdl clientAddr msg = do
  hPutStrLn connhdl msg
  withMVar lock (\a -> handlerFunc clientAddr msg >> return a)

handlerFunc :: SockAddr -> String -> IO ()
handlerFunc addr msg = do
  putStrLn $ "From " ++ show addr ++ ": " ++ msg
