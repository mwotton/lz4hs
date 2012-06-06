module Codec.Compression.LZ4 where
import Codec.Compression.LZ4.Foreign
import Foreign
import Data.ByteString
import Debug.Trace

-- Following zlib's lead, we pretend to be pure on Bytestrings
-- errors may be thrown for bad formats. I'm not entirely comfortable with this.

compress :: ByteString -> ByteString
compress x = unsafePerformIO $ useAsCStringLen x $ \(input,len) ->
  let outLen = c_LZ4_compressBound len in
  allocaBytes (1+outLen) $ \out -> do    
    written <- c_LZ4_compress input out (fromIntegral outLen)
    packCStringLen (out, written)

uncompress :: ByteString -> ByteString
uncompress x = unsafePerformIO $ useAsCStringLen x $ \(input,len) ->
  -- this is a complete punt. let's say we use 5*len for the moment,
  -- and extend to lazy bytestrings later.
  let outLen = 5 * len in
  allocaBytes (1+outLen) $ \out -> do
    written <- c_LZ4_uncompress_unknownOutputSize input out (fromIntegral len) (fromIntegral outLen)
    if written < 0
      then error "decompression failed!"
      else packCString out