module Text.XML.DOM.Parser.Class
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
  ) where

import           Control.Applicative
import           Control.Lens
import           Data.Fixed
import           Data.OpenUnion
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Typeable
import           Data.Void
import           Text.Shakespeare.Text (st)
import           Text.XML
import           Text.XML.DOM.Parser.Combinators
import           Text.XML.DOM.Parser.Types
import           TypeFun.Data.List hiding (Union)

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

instance FromDom Void where
  fromDom = voidFromDom

instance FromDom Text where
  fromDom = textFromDom

instance FromDom String where
  fromDom = stringFromDom

instance FromDom Char where
  fromDom = charFromDom

instance FromDom Int where
  fromDom = intFromDom

instance FromDom Integer where
  fromDom = integerFromDom

instance FromDom Double where
  fromDom = doubleFromDom

instance (HasResolution a, Typeable a) => FromDom (Fixed a) where
  fromDom = fixedFromDom

instance FromDom Bool where
  fromDom = boolFromDom

instance FromDom (Union '[]) where
  fromDom = empty

instance ( Typeable a, FromDom a, FromDom (Union as)
         , SubList as (a ': as) )
         => FromDom (Union (a ': as)) where
  -- fromDom :: forall m. (DomParserMonad m) => m a
  fromDom = (liftUnion <$> (proxyFromDom (Proxy :: Proxy a)))
        <|> (reUnion <$> (proxyFromDom (Proxy :: Proxy (Union as))))

instance FromDom Element where
  fromDom = elementFromDom

elementFromDom :: (Monad m) => DomParserT Identity m Element
elementFromDom = view $ pdElements . to runIdentity

unionFromDom
  :: (Monad m, FromDom (Union as))
  => proxy as
  -> DomParserT Identity m (Union as)
unionFromDom _ = fromDom

textFromDom :: (Monad m) => DomParserT Identity m Text
textFromDom = parseContent pure

stringFromDom :: (Monad m) => DomParserT Identity m String
stringFromDom = parseContent $ pure . T.unpack

charFromDom :: (Monad m) => DomParserT Identity m Char
charFromDom = parseContent $ \t -> case T.unpack $ T.strip t of
  [c] -> pure c
  _ -> throwParserError $ PEWrongFormat
    "Should have exactly one non-blank character"

intFromDom :: (Monad m) => DomParserT Identity m Int
intFromDom = parseContent readContent

integerFromDom :: (Monad m) => DomParserT Identity m Integer
integerFromDom = parseContent readContent

doubleFromDom :: (Monad m) => DomParserT Identity m Double
doubleFromDom = parseContent readContent

fixedFromDom
  :: (Monad m, Typeable a, HasResolution a)
  => DomParserT Identity m (Fixed a)
fixedFromDom = parseContent readContent

-- | Expects content to be y, yes, t, true or 1 for True value. n, no,
-- f, false or 0 for False value. Case is not significant, blank
-- characters are striped.
boolFromDom :: (Monad m) => DomParserT Identity m Bool
boolFromDom = parseContent $ \t ->
  let
    lowt  = T.toLower $ T.strip t
    tvals = ["y", "yes", "t", "true", "1"]
    fvals = ["n", "no", "f", "false", "0"]
  in if | lowt `elem` tvals -> return True
        | lowt `elem` fvals -> return False
        | otherwise         ->
          let msg = [st|Could not read "#{t}" as Bool|]
          in throwParserError $ PEWrongFormat msg

-- | Always successfully parses any DOM to @()@
unitFromDom :: (Monad m) => DomParserT Identity m  ()
unitFromDom = pure ()

-- | Never parses successfully. It is just 'mzero'
voidFromDom :: (Monad m) => DomParserT Identity m  Void
voidFromDom = empty
