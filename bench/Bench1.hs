module Main
       ( main -- :: IO ()
       ) where
import Prelude hiding (words)

import qualified Data.ByteString           as S
import qualified Codec.Compression.Snappy  as Snappy
import qualified Codec.Compression.QuickLZ as QuickLZ
import qualified Codec.Compression.LZ4     as LZ4

import Criterion.Main
import Criterion.Config
import Control.DeepSeq (NFData)


instance NFData S.ByteString


main :: IO ()
main = do
  words <- S.readFile "/usr/share/dict/words"

  let cfg = defaultConfig { cfgPerformGC = ljust True }
  defaultMainWith cfg (return ())
    [ bgroup "/usr/share/dict/words"
      [ bench "snappy"    $ nf Snappy.compress words
      , bench "quicklz"   $ nf QuickLZ.compress words
      , bench "lz4"       $ nf LZ4.compress words
      , bench "lz4 HC"    $ nf LZ4.compressHC words
      , bench "lz4 ultra" $ nf LZ4.compressPlusHC words
      ]
    ]
