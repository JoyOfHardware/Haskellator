{-# LANGUAGE Safe #-}
module Haskellator.Flags (Flag (..)) where

data Flag = FlagV
          | FlagH
          | FlagP
          | FlagO String
      deriving (Eq, Show)
