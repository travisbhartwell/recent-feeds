{-# LANGUAGE BangPatterns #-}
module Main where

import Data.Binary
import qualified Data.Map as Map
    
import System.Directory
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
  items <- deDupWithSerializedMap $ feedItems feed
  let feed' = withFeedItems items feed  
  putStrLn $ ppTopElement $ xmlFeed feed'

type IdCountMap = Map.Map String Integer

-- TODO: Need to pick a standard place for this
seenMapFile :: FilePath
seenMapFile = "seenMap.ser" 

defaultValue  :: Integer
defaultValue = 0 

deDupWithSerializedMap :: [Item] -> IO [Item]
deDupWithSerializedMap items =
    do
      hasFile <- doesFileExist seenMapFile
      !seenMap <- if hasFile
                 then
                     decodeFile seenMapFile
                 else
                     return Map.empty
      let (items', seenMap') = deDupItems items seenMap
      encodeFile seenMapFile seenMap'
      return items'

deDupItems :: [Item] -> IdCountMap -> ([Item], IdCountMap)
deDupItems items = deDupItems' items []

deDupItems' :: [Item] -> [Item] -> IdCountMap -> ([Item], IdCountMap)
deDupItems' (item:items) keptItems seenMap =
    case getDeliciousUrlId item of
      Nothing -> deDupItems' items keptItems seenMap -- We didn't get an id, so drop it and move on
      Just anId -> deDupItems' items itemsToKeep (Map.insert anId (count + 1) seenMap)
          where
            count = Map.findWithDefault defaultValue anId seenMap
            itemsToKeep = if count > 0 then keptItems else keptItems ++ [item]

deDupItems' [] keptItems seenMap =  (keptItems, seenMap)


getDeliciousUrlId :: Item -> Maybe String
getDeliciousUrlId (RSSItem item) = getDelIdFromUrl `fmap` rssItemComments item
getDeliciousUrlId _ = Nothing

getDelIdFromUrl :: String -> String
getDelIdFromUrl comments = drop baseUrlLen comments
    where
      baseUrlLen = length baseUrl
      baseUrl = "http://delicious.com/url/"

-- Local Variables:
-- compile-command: "ghc --make -Wall -o recent-feed recent-feed.hs"
-- End:
