module Text.XML.DOM.Parser.Types
  ( -- * Parser internals
    ParserError(..)
  , pePath
  , peDetails
  , ParserErrors(..)
  , _ParserErrors
  , ParserData(..)
  , pdElements
  , pdPath
    -- * Parser type
  , DomParserT
  , DomParser
  , runDomParserT
  , runDomParser
    -- * Auxiliary
  , DomTraversable(..)
  , throwParserError
  , throwWrongFormat
  ) where

import           Control.Exception
import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.List.NonEmpty as NE
import           Data.Maybe
import           Data.Text (Text)
import           GHC.Generics (Generic)
import           Text.XML
import           Text.XML.Lens

-- | DOM parser error description.
data ParserError
  -- | Tag not found which should be.
  = PENotFound
    { _pePath :: [Text]
    }

  -- | Tag contents has wrong format, (could not read text to value)
  | PEWrongFormat
    { _peDetails :: Text
    , _pePath    :: [Text]     -- ^ path of element
    }

  -- | Node should have text content, but it does not.
  | PEContentNotFound
    { _pePath :: [Text]
    }

  -- | Some other error
  | PEOther
    { _peDetails :: Text
    , _pePath    :: [Text]
    } deriving (Eq, Ord, Show, Generic)

makeLenses ''ParserError

instance Exception ParserError

newtype ParserErrors = ParserErrors
  { unParserErrors :: [ParserError]
  } deriving (Ord, Eq, Show, Monoid, Generic)

makePrisms ''ParserErrors

instance Exception ParserErrors


{- | Parser scope parser runs in. Functor argument is usually @Identity@ or
@[]@.

If functor is @Identity@ then parser expects exactly ONE current element. This
is common behavior for content parsers, or parsers expecting strict XML
structure.

If functor is @[]@ then parser expects arbitrary current elements count. This is
the case when you use combinators 'divePath' or 'diveElem' (posible other
variants of similar combinators). This kind of combinators performs search for
elements somewhere in descendants and result have arbitrary length in common
case.
-}

data ParserData f = ParserData
    { _pdElements :: f Element
      -- ^ Current element(s). Functor is intended to be either @Identity@ or
      -- @[]@
    , _pdPath     :: [Text]
      -- ^ Path for error reporting
    }

makeLenses ''ParserData

type DomParserT f m = ReaderT (ParserData f) (ExceptT ParserErrors m)
type DomParser f = DomParserT f Identity

-- | Run parser on root element of Document.
runDomParserT
  :: (Monad m)
  => Document
  -> DomParserT Identity m a
  -> m (Either ParserErrors a)
runDomParserT doc par =
  let pd = ParserData
        { _pdElements = doc ^. root . to pure
        , _pdPath     = [doc ^. root . localName]
        }
  in runExceptT $ runReaderT par pd

runDomParser
  :: Document
  -> DomParser Identity a
  -> Either ParserErrors a
runDomParser doc par = runIdentity $ runDomParserT doc par

-- | Class of traversable functors which may be constructed from list. Or may
-- not.
class Traversable f => DomTraversable f where
  -- | If method return Nothing this means we can not build traversable from
  -- given list. In this case combinator should fail traversing.
  buildDomTraversable :: [a] ->  Maybe (f a)

instance DomTraversable Identity where
  buildDomTraversable = fmap Identity . listToMaybe

instance DomTraversable [] where
  buildDomTraversable = Just

instance DomTraversable Maybe where
  buildDomTraversable = Just . listToMaybe

instance DomTraversable NonEmpty where
  buildDomTraversable = NE.nonEmpty

throwParserError
  :: (MonadError ParserErrors m, MonadReader (ParserData f) m)
  => ([Text] -> ParserError)
  -> m a
throwParserError mkerr = do
  path <- view pdPath
  throwError $ ParserErrors [mkerr path]

-- | Throw 'PEWrongFormat' as very common case
throwWrongFormat
  :: (MonadError ParserErrors m, MonadReader (ParserData f) m)
  => Text
  -> m a
throwWrongFormat err = throwParserError $ PEWrongFormat err
