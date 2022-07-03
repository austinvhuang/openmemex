module Scripts where

import DB
import CrawlTools

addTextManual dt tm entry tags = do
  entryID <- addText $ WriteText dt tm entry
  mapM_ (initTag entryID) tags

addLinkManual dt tm entry tags = do
  entryID <- addLink $ WriteLink dt tm entry
  mapM_ (initTag entryID) tags
  link <- getLink (fromIntegral entryID)
  crawlLinks link
