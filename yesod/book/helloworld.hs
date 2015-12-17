{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE ViewPatterns      #-}
{-# LANGUAGE MultiParamTypeClasses #-}

import           Data.Text      (Text)
import qualified Data.Text      as T
import           System.Process (readProcessWithExitCode)
import           System.Time    (getClockTime)
import           Yesod

data HelloWorld = HelloWorld String deriving (Show)

mkYesod "HelloWorld" [parseRoutes|
/ HomeR GET
/foo/#Int FooR GET
/person/#Text PersonR GET
/year/#Integer/month/#Text/day/#Int DateR
/wiki/*Texts WikiR GET
|]

instance Yesod HelloWorld

title :: String
title = "てすと"

getHomeR :: Handler Html
getHomeR = do
  $logInfo "もげ"
  t <- liftIO getClockTime
  (ec, stdout, stderr) <- liftIO $ readProcessWithExitCode "ls" [] ""
  yesod <- getYesod
  defaultLayout $ do
    setTitle "てすと"
    [whamlet|
     <h1>ほげ
     <p>Hello World!!
     <p>Current Time: #{show t}
     <pre>#{stdout}
     <p>Yesod: #{show yesod}
    |]

getFooR :: Int -> Handler Html
getFooR i = do
  defaultLayout $ do
    setTitle "test"
    [whamlet|
     <h1>Test
     <p>Value: #{show i}
    |]

getPersonR :: Text -> Handler Html
getPersonR name = defaultLayout [whamlet|<h1>Hello #{name}!|]

handleDateR :: Integer -> Text -> Int -> Handler Text -- text/plain
handleDateR year month day =
    return $
        T.concat [month, " ", T.pack $ show day, ", ", T.pack $ show year]

getWikiR :: [Text] -> Handler Text
getWikiR = return . T.unwords

data Car =
  Car { carModel :: Text
      , carYear  :: Int
      , carColor :: Maybe Text
      } deriving (Eq, Show)

carAForm :: Maybe Car -> AForm Handler Car
carAForm mcar = Car
    <$> areq textField "Model" (carModel <$> mcar)
    <*> areq intField  "Year"  (carYear  <$> mcar)
    <*> aopt textField "Color" (carColor <$> mcar)

carForm :: Html -> MForm Handler (FormResult Car, Widget)
carForm = renderTable $ carAForm $ Just $ Car "Forte" 2010 $ Just "gray"

instance RenderMessage HelloWorld FormMessage

main :: IO ()
main = warp 3000 (HelloWorld "Test")
