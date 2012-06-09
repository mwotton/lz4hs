{-# LANGUAGE ForeignFunctionInterface #-}
module Codec.Compression.LZ4.Foreign (
  c_LZ4_compress,
  c_LZ4_compressHC,
  c_LZ4_uncompress_unknownOutputSize, 
  c_LZ4_compressBound) where

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "lz4.h LZ4_compress"
      c_LZ4_compress :: CString -> CString -> CInt -> IO Int

foreign import ccall "lz4hc.h LZ4_compressHC"
      c_LZ4_compressHC :: CString -> CString -> CInt -> IO Int

foreign import ccall "lz4.h LZ4_uncompress_unknownOutputSize"
  c_LZ4_uncompress_unknownOutputSize :: CString -> CString -> CInt -> CInt -> IO Int

foreign import ccall "lz4.h LZ4_compressBound"
      c_LZ4_compressBound :: Int -> Int
