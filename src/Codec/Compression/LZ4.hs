module Codec.Compression.LZ4 where
import Codec.Compression.LZ4.Foreign
import Data.ByteString

-- Following zlib's lead, we pretend to be pure on Bytestrings
-- errors may be thrown for bad formats. I'm not entirely comfortable with this.

compress :: ByteString -> ByteString
compress x = x

uncompress :: ByteString -> ByteString
uncompress y = yb