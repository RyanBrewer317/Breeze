module Main ( main ) where
import Distribution.Simple.Test.LibV09 ( stubMain )
import TestParser ( tests )
main :: IO ()
main = stubMain tests
