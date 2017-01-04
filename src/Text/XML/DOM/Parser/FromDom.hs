module Text.XML.DOM.Parser.FromDom
  ( -- * FromDom
    FromDom(..)
  , proxyFromDom
    -- * Explicit methods for convenience
  , elementFromDom
  , unionFromDom
  , textFromDom
  , stringFromDom
  , charFromDom
  , intFromDom
  , integerFromDom
  , doubleFromDom
  , fixedFromDom
  , boolFromDom
  , unitFromDom
  , voidFromDom
  , scientificFromDom
  ) where

import Control.Applicative
import Control.Lens
import Data.Fixed
import Data.OpenUnion
import Data.Scientific
import Data.Text as T hiding (empty)
import Data.Typeable
import Data.Void
import Text.XML
import Text.XML.DOM.Parser.Common
import Text.XML.DOM.Parser.Types
import TypeFun.Data.List hiding (Union)

proxyFromDom
  :: forall proxy m a
   . (FromDom a, Monad m)
  => proxy a
  -> DomParserT Identity m a
proxyFromDom _ = fromDom

-- | Class of types which can be parsed from single XML element.
class FromDom a where
  fromDom :: (Monad m) => DomParserT Identity m a

instance FromDom () where
  fromDom = unitFromDom

-- | Always successfully parses any DOM to @()@
unitFromDom :: (Monad m) => DomParserT Identity m  ()
unitFromDom = pure ()

instance FromDom Void where
  fromDom = voidFromDom

-- | Never parses successfully. It is just 'empty'
voidFromDom :: (Monad m) => DomParserT Identity m  Void
voidFromDom = empty

instance FromDom Text where
  fromDom = textFromDom

textFromDom :: (Monad m) => DomParserT Identity m Text
textFromDom = parseContent Right

instance FromDom String where
  fromDom = stringFromDom

stringFromDom :: (Monad m) => DomParserT Identity m String
stringFromDom = parseContent $ Right . T.unpack

instance FromDom Char where
  fromDom = charFromDom

charFromDom :: (Monad m) => DomParserT Identity m Char
charFromDom = parseContent readChar

instance FromDom Int where
  fromDom = intFromDom

intFromDom :: (Monad m) => DomParserT Identity m Int
intFromDom = parseContent readContent

instance FromDom Integer where
  fromDom = integerFromDom

integerFromDom :: (Monad m) => DomParserT Identity m Integer
integerFromDom = parseContent readContent

instance FromDom Double where
  fromDom = doubleFromDom

doubleFromDom :: (Monad m) => DomParserT Identity m Double
doubleFromDom = parseContent readContent

instance (HasResolution a, Typeable a) => FromDom (Fixed a) where
  fromDom = fixedFromDom

fixedFromDom
  :: (Monad m, Typeable a, HasResolution a)
  => DomParserT Identity m (Fixed a)
fixedFromDom = parseContent readContent

instance FromDom Scientific where
  fromDom = scientificFromDom

scientificFromDom :: Monad m => DomParserT Identity m Scientific
scientificFromDom = parseContent readContent

instance FromDom Bool where
  fromDom = boolFromDom

-- | Expects content to be y, yes, t, true or 1 for True Values n, no,
-- f, false or 0 for False. Case is not significant, blank characters
-- are striped.
boolFromDom :: (Monad m) => DomParserT Identity m Bool
boolFromDom = parseContent readBool

instance FromDom (Union '[]) where
  fromDom = empty

instance
  ( Typeable a, FromDom a, FromDom (Union as)
  , SubList as (a ': as) )
  => FromDom (Union (a ': as)) where
  -- fromDom :: forall m. (DomParserMonad m) => m a
  fromDom = (liftUnion <$> (proxyFromDom (Proxy :: Proxy a)))
    <|> (reUnion <$> (proxyFromDom (Proxy :: Proxy (Union as))))

unionFromDom
  :: (Monad m, FromDom (Union as))
  => proxy as
  -> DomParserT Identity m (Union as)
unionFromDom _ = fromDom

instance FromDom Element where
  fromDom = elementFromDom

elementFromDom :: (Monad m) => DomParserT Identity m Element
elementFromDom = view $ pdElements . to runIdentity
