import Test.Hspec
import Test.QuickCheck
import Control.Exception
import Example

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "Test" Example.example
