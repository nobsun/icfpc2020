
import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "src/TextParser.hs"
  , "src/Modulate.hs"
  ]
