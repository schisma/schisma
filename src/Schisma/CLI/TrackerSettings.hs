{-# LANGUAGE DeriveGeneric #-}

module Schisma.CLI.TrackerSettings
  ( FrequencyMapperJSON
  , toFrequencyMapper
  ) where

import           Prelude                 hiding ( head )

import           Data.Aeson                     ( FromJSON )

import           GHC.Generics                   ( Generic )

import           Data.Map.Strict                ( Map )
import           Data.Maybe                     ( fromMaybe )
import           Data.Text                      ( Text )

import           Schisma.Tracker.Mappers        ( defaultFrequencyMapper
                                                , frequencyWithRandomDetuningMapper
                                                )

import           Schisma.Utilities              ( head )


-- TODO: Doc

data FrequencyMapperJSON = FrequencyMapperJSON
  { name      :: String
  , arguments :: [Double]
  }
  deriving (Generic, Show)

instance FromJSON FrequencyMapperJSON

toFrequencyMapper
  :: FrequencyMapperJSON
  -> (Integer -> Text -> Text -> Text -> Map Text Double)
toFrequencyMapper json = case name json of
  "withRandomDetuning" ->
    let limit = fromMaybe 0 $ head (arguments json)
    in  frequencyWithRandomDetuningMapper limit
  _ -> defaultFrequencyMapper
