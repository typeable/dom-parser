module Text.XML.DOM.Parser.FromAttribute
  ( -- * FromAttribute
    FromAttribute(..)
  , proxyFromAttribute
    -- * Explicit methods
  , textFromAttribute
  , stringFromAttribute
  , charFromAttribute
  , intFromAttribute
  , integerFromAttribute
  , doubleFromAttribute
  , fixedFromAttribute
  , boolFromAttribute
  , unitFromAttribute
  , voidFromAttribute
  , scientificFromAttribute

  ) where

import Data.Fixed
import Data.Scientific
import Data.Text as T
import Data.Typeable
import Data.Void
import Text.XML.DOM.Parser.Common

class FromAttribute a where
  fromAttribute
    :: Text
    -- ^ Attribute contents to parse
    -> Either Text a
    -- ^ Either error message or result

proxyFromAttribute
  :: FromAttribute a
  => Proxy a
  -> Text
  -> Either Text a
proxyFromAttribute _ = fromAttribute

instance FromAttribute () where
  fromAttribute = unitFromAttribute

unitFromAttribute :: Text -> Either Text ()
unitFromAttribute _ = Right ()

instance FromAttribute Void where
  fromAttribute = voidFromAttribute

voidFromAttribute :: Text -> Either Text Void
voidFromAttribute _ = Left "Void has been parsed"

instance FromAttribute Text where
  fromAttribute = textFromAttribute

textFromAttribute :: Text -> Either Text Text
textFromAttribute = Right

instance FromAttribute String where
  fromAttribute = stringFromAttribute

stringFromAttribute :: Text -> Either Text String
stringFromAttribute = Right . T.unpack

instance FromAttribute Char where
  fromAttribute = charFromAttribute

charFromAttribute :: Text -> Either Text Char
charFromAttribute = readChar

instance FromAttribute Int where
  fromAttribute = intFromAttribute

intFromAttribute :: Text -> Either Text Int
intFromAttribute = readContent

instance FromAttribute Integer where
  fromAttribute = integerFromAttribute

integerFromAttribute :: Text -> Either Text Integer
integerFromAttribute = readContent

instance FromAttribute Double where
  fromAttribute = doubleFromAttribute

doubleFromAttribute :: Text -> Either Text Double
doubleFromAttribute = readContent

instance (Typeable a, HasResolution a) => FromAttribute (Fixed a) where
  fromAttribute = fixedFromAttribute

fixedFromAttribute
  :: (HasResolution a, Typeable a)
  => Text
  -> Either Text (Fixed a)
fixedFromAttribute = readContent

instance FromAttribute Bool where
  fromAttribute = boolFromAttribute

boolFromAttribute :: Text -> Either Text Bool
boolFromAttribute = readBool

instance FromAttribute Scientific where
  fromAttribute = scientificFromAttribute

scientificFromAttribute :: Text -> Either Text Scientific
scientificFromAttribute = readContent
