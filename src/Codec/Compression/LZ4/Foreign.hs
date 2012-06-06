{-# LANGUAGE ForeignFunctionInterface #-}

module Codec.Compression.LZ4.Foreign where

import Foreign
import Foreign.C.Types
import Foreign.C.String

foreign import ccall "lz4.h LZ4_compress"
      c_LZ4_compress :: CString -> CString -> CInt -> IO ()
                        
foreign import ccall "lz4hc.h LZ4_compressHC"
      c_LZ4_compressHC :: CString -> CString -> CInt -> IO ()                        
                        
foreign import ccall "lz4.h LZ4_uncompress"
      c_LZ4_uncompress :: CString -> CString -> CInt -> IO ()                        
                          
foreign import ccall "lz4.h LZ4_compressBound"
      c_LZ4_compressBound :: Int -> Int
