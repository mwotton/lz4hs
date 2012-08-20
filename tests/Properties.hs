{-# LANGUAGE ViewPatterns #-}
module Main (main) where
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.ByteString as S
import Codec.Compression.LZ4

main :: IO ()
main = hspec $ do
  describe "compression" $ do
    prop "is pure"              $ prop_compress_pure compress decompress
    prop "(>>= decomp) = id" $ prop_compression_id compress decompress
  describe "high compression" $ do
    prop "is pure"              $ prop_compress_pure compressHC decompress
    prop "(>>= decomp) = id" $ prop_compression_id compressHC decompress
  describe "ultra compression" $ do
    prop "is pure"              $ prop_compress_pure compressPlusHC decompressPlusHC
    prop "(>>= decomp) = id" $ prop_compression_id compressPlusHC decompressPlusHC
  describe "decompression" $ do
    prop "is pure (normal)"     $ prop_decompress_pure compress decompress
    prop "is pure (high)"       $ prop_decompress_pure compressHC decompress
    prop "is pure (ultra)"      $ prop_decompress_pure compressPlusHC decompressPlusHC

prop_compress_pure comp decomp (S.pack -> xs) =
  (comp xs) == (comp xs)

prop_compression_id comp decomp (S.pack -> xs) =
  maybe False (== xs) (comp xs >>= decomp) 

prop_decompress_pure comp decomp (S.pack -> xs) =
  let z = comp xs
  in (z >>= decomp) == (z >>= decomp)


