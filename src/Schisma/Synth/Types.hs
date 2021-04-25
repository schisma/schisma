{-# LANGUAGE DeriveGeneric #-}

module Schisma.Synth.Types
  ( Synth(..)
  , SynthParameter(..)
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

data Synth = Synth
  { synthName :: Text
  , synthParameters :: [SynthParameter] }
  deriving (Generic, Show, Ord, Eq)

instance ToJSON Synth where
  toJSON = genericToJSON defaultOptions
    { fieldLabelModifier = unpack . toCamel . drop 5 . pack
    }

data SynthParameter = SynthParameter
  { synthParameterName         :: Text
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
