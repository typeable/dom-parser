module Text.XML.DOM.Parser.Attributes
  ( parseAttribute
  , parseAttributeMaybe
  , getCurrentAttributes
  , getCurrentAttribute
  ) where

import Control.Lens
import Data.Map.Strict as M
import Data.Text as T
import Text.XML.DOM.Parser.Types
import Text.XML.Lens

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

-- | Parses attribute with given name. Returns Nothing if attribute is
-- not found.
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
