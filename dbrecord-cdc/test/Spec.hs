{-# LANGUAGE TypeApplications #-}
module Main (main) where

import DBRecord.CDC
import CDCTest

main :: IO ()
main = do
  putStrLn $ createCdcType "cdc"
  putStrLn $ createCDCTable "cdc"
  putStrLn $ createCDCCapturedColumn "cdc"
  putStrLn $ createCDCDDLHistory "cdc"
  putStrLn $ createCDCData @TestDB @TestCdc "cdc" "test_cdc"
  putStrLn $ dropCDCTable "cdc"
  putStrLn $ dropCDCCapturedColumn "cdc"
  putStrLn $ dropCDCDDLHistory "cdc"
  putStrLn $ dropCDCData "cdc" "test_cdc"
  putStrLn $ dropCdcType "cdc"
  putStrLn "Test suite not yet implemented."
