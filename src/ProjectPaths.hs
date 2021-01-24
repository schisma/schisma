module ProjectPaths
  ( getIncludesDir
  )
where

import           System.FilePath                ( (</>) )

import Data.Text (Text, pack)

import           Paths_schisma                  ( getDataDir )

getIncludesDir :: IO Text
getIncludesDir = do
  dataDir <- getDataDir
  return $ pack (dataDir </> "includes/")
