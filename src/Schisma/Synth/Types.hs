{-# LANGUAGE DeriveGeneric #-}

module Schisma.Synth.Types
  ( SynthParameter(..)
  ) where

import           Prelude                 hiding ( drop )

import           GHC.Generics                   ( Generic )

import           Data.Aeson                     ( ToJSON
                                                , defaultOptions
                                                , fieldLabelModifier
                                                , genericToJSON
                                                , toJSON
                                                )

import           Data.Text                      ( Text
                                                , drop
                                                , pack
                                                , unpack
                                                )
import           Data.Text.Manipulate           ( toCamel )

data SynthParameter = SynthParameter
  { synthParameterName         :: Text
  , synthParameterDisplayName  :: Text
  , synthParameterMinimum      :: Double
  , synthParameterMaximum      :: Double
  , synthParameterStep         :: Double
  , synthParameterDefaultValue :: Double
  , synthParameterMidiCcNumber :: Integer
  }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON SynthParameter where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = unpack . toCamel . drop 14 . pack
    }
