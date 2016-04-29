{-# LANGUAGE OverloadedStrings, ScopedTypeVariables, DeriveDataTypeable #-}
import qualified Web.Scotty as S

import Data.Monoid ((<>))
import Control.Monad (forM_,forM)
import Control.Monad.IO.Class (liftIO)
import Control.Applicative

import Text.Blaze
import Text.Blaze.Html5 hiding (main)
import Text.Blaze.Html5.Attributes
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A
import Text.Blaze.Html.Renderer.Text

import Database.MySQL.Simple as Db

import qualified Data.Text as T
import Data.Text (Text)

import Data.Pool

outHtml = S.html . renderHtml

baseTemplate title' body' = docTypeHtml $ do
  H.head $ do
    H.title title'
    meta ! charset "utf-8"
  body body'

indexHandler pool = S.get "/" $ do
  items <- liftIO $ withResource pool (flip query_ "select id,name from product")
  outHtml $ baseTemplate "Главная" $ do
    table ! A.style "border: 1px solid black;" $ do
      forM_ items $ \(id'::Int,name::T.Text) -> do
        tr $ do
          td $ toHtml id'
          td $ toHtml name

main = do
  let createConn = connect defaultConnectInfo {
                                        connectUser = "mike",
                                        connectPassword = "",
                                        connectDatabase = "mike"
                                      }
  pool <- createPool createConn close 1 10 120
  S.scotty 3000 $ do
    indexHandler pool
