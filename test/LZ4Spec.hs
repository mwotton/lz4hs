module LZ4Spec(spec) where
-- import Test.Hspec
import Test.Hspec.QuickCheck
import Test.Hspec.ShouldBe
import Test.QuickCheck hiding (property)
-- import qualified Test.Hspec.Monadic
import Codec.Compression.LZ4



spec :: Specs
spec = do

  describe "reverse" $ do
    it "can compress" $ do
      compress "a" == compress "a"
    it "can roundtrip" $ do
      (property $ forAll string $
       \s -> (compress $ uncompress s) == s)
       
-- FIX really want a better random string algo
string :: Gen String
string = do
   nums <- elements [1..1000]
   vectorOf nums (elements ['A' .. 'z'])