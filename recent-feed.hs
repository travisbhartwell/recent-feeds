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
  feed <- parseFeedFromFile filename
  items <- return $ deDupItems $ feedItems feed
  feed <- return $ withFeedItems items feed
  putStrLn $ ppTopElement $ xmlFeed feed
    where
      filename = "del.rss"
      

type IdCountMap = Map.Map String Integer

defaultValue = 0 :: Integer
               
deDupItems :: [Item] -> [Item]
deDupItems items = deDupItems' items [] Map.empty

deDupItems' :: [Item] -> [Item] -> IdCountMap -> [Item]
deDupItems' (item:items) keptItems seenMap =
    case delId of
      Nothing -> deDupItems' items keptItems seenMap -- We didn't get an id, so drop it and move on
      Just anId ->
          if count > 0
          then
              -- Already seen, ignore it
              deDupItems' items keptItems (Map.insert anId (count + 1) seenMap)              
          else
              -- Haven't seen it
              deDupItems' items (keptItems ++ [item]) (Map.insert anId (count + 1) seenMap)
          where
            count = Map.findWithDefault defaultValue anId seenMap
    where
      delId = getDeliciousUrlId item

deDupItems' [] keptItems seenMap = keptItems
    
          
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
