module Main where

import           Control.Exception
import           Control.Lens
import           Control.Monad
import           Data.Default
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Monoid
import           Data.Text (Text)
import           Test.Hspec
import           Text.Shakespeare.Text (lt)
import           Text.XML
import           Text.XML.DOM.Parser


data TestStructure = TestStructure
  { tsName  :: Text
  , tsInts  :: [Int]
  , tsBools :: NonEmpty Bool
  } deriving (Eq, Show)

testStructureFromDom :: (Monad m) => DomParserT Identity m TestStructure
testStructureFromDom = do
  tsName <- inElem "name" fromDom
  tsInts <- inElemAll "int" intFromDom
  tsBools <- inElemNe "bool" boolFromDom
  return TestStructure{..}

instance FromDom TestStructure where
  fromDom = testStructureFromDom

docStruct :: Document
docStruct = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <name>name</name>
  <int>1</int>
  <int>2</int>
  <bool>t</bool>
</root>
|]

docNone :: Document
docNone = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
</root>
|]

docEmpty :: Document
docEmpty = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a/>
</root>
|]

docSimple :: Document
docSimple = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a>content</a>
</root>
|]

docMultiple :: Document
docMultiple = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a>content1</a>
  <a>content2</a>
  <a>content3</a>
</root>
|]

docDeep :: Document
docDeep = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a><b>content</b></a>
</root>
|]

docDeepWithContent :: Document
docDeepWithContent = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a>content<b>content</b>content</a>
</root>
|]

docDeepMultiple1 :: Document
docDeepMultiple1 = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a>
    <b>content1</b>
    <b>content2</b>
  </a>
  <a>
    <b>content3</b>
  </a>
  <a/>
</root>
|]

docDeepMultiple2 :: Document
docDeepMultiple2 = parseText_ def [lt|
<?xml version="1.0" encoding="utf-8"?>
<root>
  <a/>
  <a>
    <b>content1</b>
  </a>
  <a>
    <b>content2</b>
    <b>content3</b>
  </a>
</root>
|]

specParser
  :: String                     -- ^ Name of spec
  -> Document                   -- ^ Document to parse
  -> (a -> Maybe String)        -- ^ Value checker, Nothing if all ok
  -> DomParser Identity a                -- ^ Parser itself
  -> Spec
specParser name doc check parser = it name $ do
  result <- either throwIO return $ runDomParser doc parser
  case check result of
    Nothing -> return ()
    Just e  -> throwIO $ ErrorCall e

specParserEq
  :: (Eq a, Show a)
  => String            -- ^ Name of spec
  -> Document          -- ^ Document to parse
  -> a                 -- ^ Value parser should return
  -> DomParser Identity a       -- ^ Parser itself
  -> Spec
specParserEq name doc a parser = specParser name doc check parser
  where
    check x | x == a = Nothing
            | otherwise = Just $ "should be " ++ show a
                          ++ " but got " ++ show x

specParserFailed
  :: (Show a)
  => String
  -> Document
  -> DomParser Identity a
  -> Spec
specParserFailed name doc parser = it name $ example $ do
  let result = runDomParser doc parser
  case result of
    Right a -> fail $ "Expected parser to fail, but returned" ++ (show a)
    Left _  -> return ()

specParserFailedPath
  :: (Show a)
  => String                     -- ^ Name of spec
  -> Document
  -> [Text]                     -- ^ Expteced path in error
  -> DomParser Identity a
  -> Spec
specParserFailedPath name doc path parser = it name $ example $ do
  let result = runDomParser doc parser
  case result of
    Right a   -> fail $ "Expected parser to fail, but returned" ++ (show a)
    Left (ParserErrors errs) -> case errs ^? folded . pePath of
      Nothing -> fail $ "Parser failed, but got no errors to analyze path"
      Just p | p == path -> return ()
             | otherwise -> fail $ "Path is not exptected: " <> show p

combinationsSpec :: Spec
combinationsSpec = do
  describe "inElem" $ do
    let parser = inElem "a" textFromDom
    describe "succeeds" $ do
      specParserEq "docSimple" docSimple "content" parser
      specParserEq "docMultiple" docMultiple "content1" parser
    describe "fails" $ do
      specParserFailed "docNone" docNone parser
      specParserFailed "docEmpty" docEmpty parser

  describe "inElemAll" $ do
    let parser = inElemAll "a" textFromDom
    describe "succeeds" $ do
      specParserEq "docNone" docNone [] parser
      specParserEq "docSimple" docSimple ["content"] parser
      specParserEq "docMultiple" docMultiple
        ["content1", "content2", "content3"] parser
    describe "fails" $ do
      specParserFailed "docEmpty" docEmpty parser

  describe "inElemMay" $ do
    let parser = inElemMay "a" textFromDom
    describe "succeeds" $ do
      specParserEq "docNone" docNone Nothing parser
      specParserEq "docSimple" docSimple (Just "content") parser
      specParserEq "docMultiple" docMultiple (Just "content1") parser
    describe "fails" $ do
      specParserFailed "docEmpty" docEmpty parser

  describe "inElemNe" $ do
    let parser = inElemNe "a" textFromDom
    describe "succeeds" $ do
      specParserEq "docSimple" docSimple (pure "content") parser
      specParserEq "docMultiple" docMultiple
        (NE.fromList ["content1", "content2", "content3"]) parser
    describe "fails" $ do
      specParserFailed "docNone" docNone parser
      specParserFailed "docEmpty" docEmpty parser

  describe "diveElem" $ do
    describe "single element" $ do
      let parser = diveElem "a" $ inElem "b" textFromDom
      describe "succeeds" $ do
        specParserEq "docDeepMultiple1" docDeepMultiple1 "content1" parser
        specParserEq "docDeepMultiple2" docDeepMultiple2 "content1" parser
      describe "fails" $ do
        specParserFailedPath "docSimple" docSimple ["root", "a", "b"] parser
    describe "multiple elements" $ do
      let
        parser = diveElem "a" $ inElemAll "b" textFromDom
        result = ["content1", "content2", "content3"]
      describe "succeeds" $ do
        specParserEq "docDeepMultiple1" docDeepMultiple1 result parser
        specParserEq "docDeepMultiple2" docDeepMultiple2 result parser
        specParserEq "docSimple" docSimple [] parser

  describe "ignoreEmpty" $ do
    describe "mandatory element" $ do
      let parser = inElem "a" $ ignoreEmpty textFromDom
      describe "succeeds" $ do
        specParserEq "docEmpty" docEmpty Nothing parser
        specParserEq "docSimple" docSimple (Just "content") parser
        specParserEq "docMultiple" docMultiple (Just "content1") parser
      describe "fails" $ do
        specParserFailed "docNone" docNone parser
    describe "optional element" $ do
      let parser = fmap join $ inElemMay "a" $ ignoreEmpty textFromDom
      describe "succeeds" $ do
        specParserEq "docNone" docNone Nothing parser
        specParserEq "docEmpty" docEmpty Nothing parser
        specParserEq "docSimple" docSimple (Just "content") parser
        specParserEq "docMultiple" docMultiple (Just "content1") parser

  describe "checkCurrentName" $ do
    describe "succeeded" $ do
      specParserEq "root element" docSimple () $ do
        checkCurrentName "root"
      specParserEq "inner element" docSimple () $ do
        inElem "a" $ do
          checkCurrentName "a"
    describe "fails" $ do
      specParserFailedPath "root element" docSimple ["toor"] $ do
        checkCurrentName "toor"
      specParserFailedPath "inner element" docSimple ["root", "b"] $ do
        inElem "a" $ checkCurrentName "b"

contentSpec :: Spec
contentSpec = do
  describe "fails if inner element found" $ do
    specParserFailed "docDeep" docDeep $ do
      inElem "a" $ textFromDom
    specParserFailed "docDeepWithContent" docDeepWithContent $ do
      inElem "a" $ textFromDom

structSpec :: Spec
structSpec = do
  describe "succeeds" $ do
    let
      result = TestStructure
                 { tsName = "name"
                 , tsInts = [1,2]
                 , tsBools = pure True }
    specParserEq "docStruct" docStruct result fromDom
  describe "fails" $ do
    specParserFailed "docSimple" docSimple testStructureFromDom

main :: IO ()
main = hspec $ do
  describe "combinators" $ do
    combinationsSpec
  describe "content parsing" $ do
    contentSpec
  describe "expected struct" $ do
    structSpec
