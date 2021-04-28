{-# LANGUAGE OverloadedStrings #-}

module Lib
    ( someFunc,
      journalFeed,
    ) where

import Data.Maybe
import Network.HTTP.Req
import Text.Feed.Import
import Text.Feed.Types
import Text.RSS.Syntax

someFunc :: IO ()
someFunc = (runReq defaultHttpConfig journalFeed) >>= (putStrLn . show)

journalFeed :: (MonadHttp m) => m (Maybe RSS)
journalFeed = do
  response <- req
         GET
         (https "dblp.dagstuhl.de" /: "news" /: "jour-update.rss")
         NoReqBody
         lbsResponse
         mempty
  return $ parseFeedSource (responseBody response) >>= extract
    where
      extract :: Feed -> Maybe RSS
      extract (RSSFeed feed) = Just feed
      extract _ = Nothing

