module Text.XML.DOM.Parser.Types
  (-- * Element matching
    ElemMatcher(..)
  , emMatch
  , emShow
  , matchElemName
  , elMatch
    -- * Name matching
  , NameMatcher(..)
  , nmMatch
  , nmShow
  , matchName
  , matchLocalName
  , matchCILocalName
    -- * Parser internals
  , DomPath(..)
  , ParserError(..)
  , pePath
  , peDetails
  , peAttributeName
  , ParserErrors(..)
  , _ParserErrors
  , ParserData(..)
  , pdElements
  , pdPath
  , DomParserT
  , DomParser
  , runDomParserT
  , runDomParser
    -- * Auxiliary
  , throwParserError
  ) where

import Control.Exception
import Control.Lens
import Control.Monad.Except
import Control.Monad.Reader
import Data.CaseInsensitive as CI
import Data.Semigroup
import Data.String
import Data.Text as T
import GHC.Generics (Generic)
import Text.XML
import Text.XML.Lens

-- | Arbitrary element matcher
--
-- @since 2.0.0
data ElemMatcher = ElemMatcher
  { _emMatch :: Element -> Bool
  , _emShow  :: Text
    -- ^ Field for 'Show' instance and bulding usefull errors
  }

makeLenses ''ElemMatcher

-- | Instance using instance of 'NameMatcher'
instance IsString ElemMatcher where
  fromString = matchElemName . fromString

instance Show ElemMatcher where
  show = T.unpack . _emShow

-- | Match element by name
--
-- @since 2.0.0
matchElemName :: NameMatcher -> ElemMatcher
matchElemName (NameMatcher matchName showName) = ElemMatcher
  { _emMatch = views name matchName
  , _emShow  = showName
  }

-- | Match over elements
--
-- @since 2.0.0
elMatch :: ElemMatcher -> Traversal' Element Element
elMatch (ElemMatcher match _) = filtered match

-- | Arbitrary name matcher. Match name any way you want, but
-- considered to be used as comparator with some name with some rules
--
-- @since 2.0.0
data NameMatcher = NameMatcher
  { _nmMatch :: Name -> Bool
    -- ^ Name matching function, usually should be simple comparsion
    -- function takin in account only local name or other components
    -- of 'Name'
  , _nmShow :: Text
    -- ^ Field for 'Show' instance and bulding usefull errors
  }

-- | Instance use 'matchCILocalName' as most general and liberal
-- matching strategy (while XML is often malformed).
--
-- @since 2.0.0
instance IsString NameMatcher where
  fromString = matchCILocalName . T.pack

instance Show NameMatcher where
  show = T.unpack . _nmShow

-- | Makes matcher which matches only local part of name igoring
-- namespace and prefix. Local name matching is case sensitive.
--
-- @since 2.0.0
matchLocalName :: Text -> NameMatcher
matchLocalName tname = NameMatcher
  { _nmMatch = \n -> nameLocalName n == tname
  , _nmShow  = tname
  }

-- | Makes matcher which matches only local part of name igoring
-- namespace and prefix. Local name matching is case insensitive. This
-- is the most common case.
--
-- @since 2.0.0
matchCILocalName :: Text -> NameMatcher
matchCILocalName tname = NameMatcher
  { _nmMatch = \n -> CI.mk (nameLocalName n) == CI.mk tname
  , _nmShow  = tname
  }

makeLenses ''NameMatcher

-- | Makes matcher which match name by 'Eq' with given
--
-- @since 2.0.0
matchName :: Name -> NameMatcher
matchName n = NameMatcher
  { _nmMatch = (== n)
  , _nmShow  = nameLocalName n
  }

-- | Path some element should be found at. Path starts from the root
-- element of the document. Errors are much more usefull with path.
newtype DomPath = DomPath
  { unDomPath :: [Text]
  } deriving (Eq, Ord, Show, Semigroup, Monoid)

-- | DOM parser error description.
data ParserError
  -- | Tag not found which should be.
  = PENotFound
    { _pePath :: DomPath
      -- ^ Path of element error occured in
    }

  -- | Expected attribute but not found
  --
  -- @since 1.0.0
  | PEAttributeNotFound
    { _peAttributeName :: NameMatcher
    , _pePath          :: DomPath
    }

  -- | Could not parse attribute
  --
  -- @since 1.0.0
  | PEAttributeWrongFormat
    { _peAttributeName :: NameMatcher
    , _peDetails       :: Text
    , _pePath          :: DomPath
    }

  -- | Node should have text content, but it does not.
  | PEContentNotFound
    { _pePath :: DomPath
    }

  -- | Tag contents has wrong format, (could not read text to value)
  | PEContentWrongFormat
    { _peDetails :: Text
    , _pePath    :: DomPath
    }

  -- | Some other error
  | PEOther
    { _peDetails :: Text
    , _pePath    :: DomPath
    } deriving (Show, Generic)

makeLenses ''ParserError

instance Exception ParserError

newtype ParserErrors = ParserErrors
  { unParserErrors :: [ParserError]
  } deriving (Show, Semigroup, Monoid, Generic)

makePrisms ''ParserErrors

instance Exception ParserErrors


{- | Parser scope.

Functor argument is usually @Identity@ or @[]@.

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
    , _pdPath     :: DomPath
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
  let
    pd = ParserData
      { _pdElements = doc ^. root . to pure
      , _pdPath     = DomPath [doc ^. root . name . to nameLocalName]
      }
  in runExceptT $ runReaderT par pd

runDomParser
  :: Document
  -> DomParser Identity a
  -> Either ParserErrors a
runDomParser doc par = runIdentity $ runDomParserT doc par

throwParserError
  :: (MonadError ParserErrors m, MonadReader (ParserData f) m)
  => (DomPath -> ParserError)
  -> m a
throwParserError mkerr = do
  path <- view pdPath
  throwError $ ParserErrors $ [mkerr path]
