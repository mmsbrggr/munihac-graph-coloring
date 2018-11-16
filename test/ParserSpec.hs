module ParserSpec where

import           Control.Monad         (forM_) 
import           Data.Either           (isLeft) 
import           Data.Text             (pack) 
import           Test.Hspec            (Spec, describe, it, shouldBe,                                         shouldSatisfy) 
import qualified Text.Parsec           as P 
import           Text.Parsec.String    (Parser) 
import qualified Parser                as PA

parseWithLeftOver :: Parser a -> String -> Either P.ParseError (a, String) 
parseWithLeftOver p = P.parse ((,) <$> p <*> leftOver) "timesheet parsing test"   
    where 
        leftOver = P.manyTill P.anyToken P.eof  

parseWithEof :: Parser a -> String -> Either P.ParseError a 
parseWithEof p = P.parse (p <* P.eof) "graph parsing test"

comment :: String
comment = "c whatevs \nc more whatevs \n"

fileInfo :: String
fileInfo = "p edge 5 5\n"

spec :: Spec
spec = do
    describe "comments" $
       it "comment gets skipped" $
            parseWithEof PA.header comment `shouldBe` Right ()
    describe "number of nodes" $
        it "I get the number of nodes" $
            parseWithEof PA.fileInfo fileInfo `shouldBe` Right 5
