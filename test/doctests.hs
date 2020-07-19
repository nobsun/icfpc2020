
import Test.DocTest

main :: IO ()
main = doctest
  [ "-package attoparsec"
  , "-isrc"
  , "src/Draw.hs"
  , "src/Eval.hs"
  , "src/GalaxyTxt.hs"
  , "src/ListDesugar.hs"
  , "src/Message.hs"
  , "src/Modulate.hs"
  , "src/TextParser.hs"
  ]
