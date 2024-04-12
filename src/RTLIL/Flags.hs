{-# LANGUAGE Safe #-}
module RTLIL.Flags (Flag (..)) where

data Flag = FlagV
          | FlagH
          | FlagO String
      deriving (Eq, Show)
