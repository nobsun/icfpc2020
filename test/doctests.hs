
import Test.DocTest

main :: IO ()
main = doctest
  [ "src/TextParser.hs"
  , "src/Modulate.hs"
  ]

