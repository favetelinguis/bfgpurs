module BussExample where

import Prelude

import Data.Either (Either(..))
import Data.String (toLower, toUpper)
import Data.Tuple (Tuple(..))
import Effect (Effect)
import Effect.Aff (Aff, forkAff, launchAff_, try)
import Effect.Aff.Bus (BusW, BusR)
import Effect.Aff.Bus as Bus
import Effect.Class.Console (log)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile)

readAFile :: BusW String -> Aff Unit
readAFile fileBus = do 
    result <- try $ readTextFile ASCII "test.txt"
    case result of 
        Right text -> Bus.write text fileBus
        Left err -> log $ show err

processFile :: (String -> String) -> BusR String -> Aff Unit
processFile convert fileBus = do 
    text <- Bus.read fileBus
    log $ convert text

main :: Effect Unit
main = launchAff_ do
  fileBus <- Bus.make
  let (Tuple readBus writeBus) = Bus.split fileBus
  void $ forkAff $ processFile toUpper readBus
  void $ forkAff $ processFile toLower readBus
  void $ forkAff $ readAFile writeBus