module Text.XML.DOM.Parser.Common
  ( -- * Buildable
    Buildable(..)
    -- * Content readers
  , readBool
  , readChar
    -- ** Reader constructors
  , maybeReadContent
  , readContent
    -- * Getting current element's properties
  , getCurrentName
  , getCurrentContent
  , getCurrentAttributes
  , getCurrentAttribute
  , checkCurrentName
    -- * Parsing element's content
  , parseContent
    -- * Parsing attributes
  , parseAttribute
  , parseAttributeMaybe
  ) where

import Control.Lens
import Control.Monad
import Control.Monad.Except
import Data.List as L
import Data.List.NonEmpty as NE
import Data.Map.Strict as M
import Data.Maybe
import Data.Monoid
import Data.Text as T
import Data.Typeable
import Text.Read
import Text.XML.DOM.Parser.Types
import Text.XML.Lens

-- | Class of traversable functors which may be constructed from list. Or may
-- not.
class Traversable f => Buildable f where
  -- | If method return Nothing this means we can not build
  -- traversable from given list. In this case 'inFilteredTrav' should
  -- fail traversing.
  build :: [a] ->  Maybe (f a)

instance Buildable Identity where
  build = fmap Identity . listToMaybe

instance Buildable [] where
  build = Just

instance Buildable Maybe where
  build = Just . listToMaybe

instance Buildable NonEmpty where
  build = NE.nonEmpty

-- | @since 1.0.0
readBool :: Text -> Either Text Bool
readBool t =
  let
    lowt  = T.toLower $ T.strip t
    tvals = ["y", "yes", "t", "true", "1"]
    fvals = ["n", "no", "f", "false", "0"]
  in if
    | lowt `elem` tvals -> Right True
    | lowt `elem` fvals -> Right False
    | otherwise         ->
        Left $ "Could not read " <> t <> " as Bool"

-- | Expects text to be single character
--
-- @since 1.0.0
readChar :: Text -> Either Text Char
readChar t = case T.unpack $ T.strip t of
  [c] -> Right c
  _   -> Left "Should have exactly one non-blank character"

-- | If reader returns 'Nothing' then resulting function returns 'Left
-- "error message"'
--
-- @since 1.0.0
maybeReadContent
  :: forall a
   . (Typeable a)
  => (Text -> Maybe a)
   -- ^ Content or attribute reader
  -> Text
   -- ^ Content or attribute value
  -> Either Text a
maybeReadContent f t = maybe (Left msg) Right $ f t
  where
    msg = "Not readable " <> n <> ": " <> t
    n = T.pack $ show $ typeRep (Proxy :: Proxy a)


-- | Tries to read given text to value using 'Read'. Useful to use
-- with 'parseContent' and 'parseAttribute'
readContent
  :: (Read a, Typeable a)
  => Text
  -> Either Text a
readContent = maybeReadContent $ readMaybe . T.unpack . T.strip

-- | Returns name of current element.
--
-- @since 1.0.0
getCurrentName :: (Monad m) => DomParserT Identity m Name
getCurrentName = view $ pdElements . to runIdentity . name

-- | If name of current tag differs from first argument throws 'PENotFound' with
-- tag name replaced in last path's segment. Useful for checking root
-- document's element name.
checkCurrentName
  :: (Monad m)
  => NameMatcher
  -> DomParserT Identity m ()
checkCurrentName n = do
  cn <- getCurrentName
  unless ((n ^. nmMatch) cn) $ do
    p <- view pdPath
    let pinit = if L.null (unDomPath p) then [] else L.init $ unDomPath p
    throwError $ ParserErrors [PENotFound $ DomPath $ pinit ++ [_nmShow n]]
  return ()

-- | Get current content. If current element contains no content or
-- have inner elements then Nothing returned
--
-- @since 1.0.0
getCurrentContent :: (Monad m) => DomParserT Identity m (Maybe Text)
getCurrentContent = do
  nds <- view $ pdElements . to runIdentity . nodes
  let
    els :: [Element]
    els = nds ^.. folded . _Element
    conts :: [Text]
    conts = nds ^.. folded . _Content
  return $ if
    | not $ L.null els -> Nothing
    | L.null conts     -> Nothing
    | otherwise      -> Just $ mconcat conts

-- | Parses content inside current tag. It expects current element set
-- consists of exactly ONE element.
parseContent
  :: (Monad m)
  => (Text -> Either Text a)
     -- ^ Content parser, return error msg if value is not parsed
  -> DomParserT Identity m a
parseContent parse = getCurrentContent >>= \case
  Nothing -> throwParserError PEContentNotFound
  Just c  -> case parse c of
    Left e  -> throwParserError $ PEWrongFormat e
    Right a -> return a

-- | Retuns map of attributes of current element
--
-- @since 1.0.0
getCurrentAttributes
  :: (Monad m)
  => DomParserT Identity m (M.Map Name Text)
getCurrentAttributes = view $ pdElements . to runIdentity . attrs

-- | Returns element with given name or 'Nothing'
--
-- @since 1.0.0
getCurrentAttribute
  :: (Monad m)
  => NameMatcher
  -> DomParserT Identity m (Maybe Text)
getCurrentAttribute attrName
  = preview $ pdElements . to runIdentity . attrs . to M.toList
  . traversed . filtered (views _1 (attrName ^. nmMatch)) . _2

-- | Parses attribute with given name, throws error if attribute is not found.
--
-- @since 1.0.0
parseAttribute
  :: (Monad m)
  => NameMatcher
     -- ^ Attribute name
  -> (Text -> Either Text a)
     -- ^ Attribute content parser
  -> DomParserT Identity m a
parseAttribute attrName parser =
  parseAttributeMaybe attrName parser >>= \case
    Nothing -> throwParserError $ PEAttributeNotFound attrName
    Just a  -> return a

-- | Parses attribute with given name.
--
-- @since 1.0.0
parseAttributeMaybe
  :: (Monad m)
  => NameMatcher
     -- ^ Attribute name
  -> (Text -> Either Text a)
     -- ^ Attribute content parser
  -> DomParserT Identity m (Maybe a)
parseAttributeMaybe attrName parser =
  getCurrentAttribute attrName >>= \case
    Nothing   -> return Nothing
    Just aval -> case parser aval of
      Left err -> throwParserError $ PEAttributeWrongFormat attrName err
      Right a  -> return $ Just a
