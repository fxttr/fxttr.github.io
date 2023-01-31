import Test.HUnit
import Test.Framework
import Test.Framework.Providers.HUnit
import Data.Monoid
import Control.Monad
import Libcrawl.Plain.Client

plainCrawlSingleTest :: Assertion
plainCrawlSingleTest = crawl ["https://google.de"]

main :: IO ()
main = defaultMainWithOpts
       [testCase "plain-crawl-single" plainCrawlSingleTest]
       mempty