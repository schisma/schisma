module ProjectPaths
  ( getIncludesDir
  ) where

getIncludesDir :: IO FilePath
getIncludesDir = do
  return "includes/"
