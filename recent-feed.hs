module Main where

import Text.Feed.Constructor
import Text.Feed.Export
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax (rssItemComments)
import Text.XML.Light 
    
main :: IO ()
main = do
  feed <- parseFeedFromFile filename
  items <- return $ feedItems feed
  items <- return $ take 1 items
  feed <- return $ withFeedItems items feed
  putStrLn $ ppTopElement $ xmlFeed feed
    where
      filename = "del.rss"

getDeliciousUrlId :: Item -> Maybe String
getDeliciousUrlId (RSSItem item) =
    case comments of
      Nothing -> Nothing
      Just value -> Just $ getDelIdFromUrl value
    where
      comments = rssItemComments item

getDelIdFromUrl :: String -> String
getDelIdFromUrl comments = drop baseUrlLen comments
    where
      baseUrlLen = length baseUrl
      baseUrl = "http://delicious.com/url/"
