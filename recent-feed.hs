module Main where

-- Eventually, I probably want to try to use bytestring-trie's
import qualified Data.Map as Map 

import Text.Feed.Constructor
import Text.Feed.Export
import Text.Feed.Import
import Text.Feed.Query
import Text.Feed.Types
import Text.RSS.Syntax (rssItemComments)
import Text.XML.Light 
    
main :: IO ()
main = do
  feed <- parseFeedFromFile "del.rss"
  items <- return $ deDupItems $ feedItems feed
  feed <- return $ withFeedItems items feed
  putStrLn $ ppTopElement $ xmlFeed feed

type IdCountMap = Map.Map String Integer

defaultValue = 0 :: Integer
               
deDupItems :: [Item] -> [Item]
deDupItems items = deDupItems' items [] Map.empty

deDupItems' :: [Item] -> [Item] -> IdCountMap -> [Item]
deDupItems' (item:items) keptItems seenMap =
    case getDeliciousUrlId item of
      Nothing -> deDupItems' items keptItems seenMap -- We didn't get an id, so drop it and move on
      Just anId -> deDupItems' items itemsToKeep (Map.insert anId (count + 1) seenMap)
          where
            count = Map.findWithDefault defaultValue anId seenMap
            itemsToKeep = if count > 0 then keptItems else keptItems ++ [item] 

deDupItems' [] keptItems seenMap = keptItems
    
          
getDeliciousUrlId :: Item -> Maybe String
getDeliciousUrlId (RSSItem item) =
    case rssItemComments item of
      Nothing -> Nothing
      Just value -> Just $ getDelIdFromUrl value

getDelIdFromUrl :: String -> String
getDelIdFromUrl comments = drop baseUrlLen comments
    where
      baseUrlLen = length baseUrl
      baseUrl = "http://delicious.com/url/"
