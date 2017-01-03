module Text.XML.DOM.Parser.FromAttribute
  ( -- * FromAttribute
    FromAttribute(..)
  ) where

import Data.Fixed
import Data.Scientific
import Data.Text as T
import Data.Typeable
import Text.XML.DOM.Parser.Common

class FromAttribute a where
  fromAttribute
    :: Text
    -- ^ Attribute contents to parse
    -> Either Text a
    -- ^ Either error message or result

instance FromAttribute () where
  fromAttribute _ = Right ()

instance FromAttribute Text where
  fromAttribute = Right

instance FromAttribute Char where
  fromAttribute = readChar

instance FromAttribute Int where
  fromAttribute = readContent

instance FromAttribute Integer where
  fromAttribute = readContent

instance (Typeable a, HasResolution a) => FromAttribute (Fixed a) where
  fromAttribute = readContent

instance FromAttribute Scientific where
  fromAttribute = readContent
