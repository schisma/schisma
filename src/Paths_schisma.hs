module Paths_schisma
  ( getDataDir
  )
where

getDataDir :: IO FilePath
getDataDir = pure "data"
