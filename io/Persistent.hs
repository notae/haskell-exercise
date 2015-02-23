{-# LANGUAGE FlexibleContexts #-}

module Persistent where

import Control.Monad.State (MonadState)
import Control.Monad.State (StateT)
import Control.Monad.State (execStateT)
import Control.Monad.State (get)
import Control.Monad.State (gets)
import Control.Monad.State (modify)
import Control.Monad.Trans (liftIO)

data Doc =
  Doc
  { items :: [Item]
  , nextItemID :: ItemID }
  deriving (Show, Read, Eq, Ord)

data Item =
  Item
  { itemID    :: ItemID
  , itemTitle :: Title
  , itemBody  :: Body
  } deriving (Show, Read, Eq, Ord)

type ItemID = Int
type Title = String
type Body = String

main :: IO ()
main = do
  putStrLn "Started."
  doc <- execStateT cmdLoop newDoc
  putStrLn "Doc:"
  print doc
  putStrLn "Terminated."
  return ()

cmdLoop :: StateT Doc IO ()
cmdLoop = do
  liftIO $ putStr "cmd> "
  cmd <- liftIO getLine
  case cmd of
   "quit" -> return ()
   "show" -> do doc <- get
                liftIO $ print doc
                cmdLoop
   "add"  -> do liftIO $ putStr "Title: "
                title <- liftIO $ getLine
                liftIO $ putStr "Body: "
                body <- liftIO $ getLine
                addItem title body
                cmdLoop
   '@' : _ -> do liftIO $ putStrLn cmd
                 cmdLoop
   "help" -> do { liftIO putHelp; cmdLoop }
   _ -> do liftIO $ putStrLn "Invalid command."
           cmdLoop

cmds :: [String]
cmds = ["quit", "show", "add", "help"]
putHelp :: IO ()
putHelp = do
  putStrLn "Available commands:"
  flip mapM_ cmds $ \cmd -> putStrLn ("  " ++ cmd)

-- TBD: connect to persitent store
newDoc :: Doc
newDoc = Doc { items = [], nextItemID = 0 }

addItem :: MonadState Doc m => Title -> Body -> m ()
addItem title body = do
  iid <- gets nextItemID
  modify $ \doc -> Doc { items = Item iid title body : items doc,
                         nextItemID = iid + 1 }

-- TBD: showItem
-- TBD: updateItem
-- TBD: deleteItem
