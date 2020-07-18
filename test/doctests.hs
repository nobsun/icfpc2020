
import Test.DocTest

main :: IO ()
main = doctest
  [ "-package attoparsec"
  , "-isrc"
  , "src/TextParser.hs"
  , "src/Modulate.hs"
  , "src/Draw.hs"
  ]
