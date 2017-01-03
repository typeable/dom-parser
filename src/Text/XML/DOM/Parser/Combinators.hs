module Text.XML.DOM.Parser.Combinators
  ( -- * Generic combinators to traverse descendants
    traverseElems
  , inFilteredTrav
    -- * Using 'DomTraversable'
  , inElemTrav
  , inElem
  , inElemAll
  , inElemMay
  , inElemNe
    -- * Dive combinators
  , divePath
  , diveElem
    -- * Explicit ignoring elements
  , ignoreElem
  , ignoreEmpty
  , ignoreBlank
    -- * Getting current element's properties
  , getCurrentName
  , getCurrentContent
  , getCurrentAttributes
  , getCurrentAttribute
    -- * Current element's checks
  , checkCurrentName
    -- * Parsing element's content
  , parseContent
  , readContent
  , maybeReadContent
    -- * Parsing attributes
  , parseAttribute
  , parseAttributeMaybe
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List.NonEmpty (NonEmpty(..))
import qualified Data.Map.Strict as M
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Data.Typeable
import           Text.Read
import           Text.XML
import           Text.XML.DOM.Parser.Types
import           Text.XML.Lens


-- | Generic function to traverse arbitrary inner cursors.
traverseElems
  :: (Monad m, Foldable g, Traversable f)
  => ([Element] -> DomParserT g m (f (DomPath, Element)))
     -- ^ Takes list of current elements and returns container with
     -- pairs of subpath (relatively to current element) and element
     -- to run parser in
  -> DomParserT Identity m a
     -- ^ Parser will be runned for each element found in traversable
  -> DomParserT g m (f a)
traverseElems trav parser = do
  pd <- ask
  inner <- trav $ pd ^.. pdElements . folded
  for inner $ \(subpath, e) -> do
    let
      newpd = ParserData
        { _pdElements = Identity e
        , _pdPath     = pd ^. pdPath <> subpath }
    -- type of reader has been changed, local won't work here
    magnify (to $ const newpd) parser

-- | Takes function filtering
inFilteredTrav
  :: (Monad m, Foldable g, DomTraversable f)
  => ([Element] -> (DomPath, [Element]))
   -- ^ Function returning some filtered elements with path suffixes which will
   -- be appended to parser's state
  -> DomParserT Identity m a
  -> DomParserT g m (f a)
inFilteredTrav deeper = traverseElems trav
  where
    trav e = do
      let (path, elems) = deeper e
      case buildDomTraversable elems of
        Nothing -> throwParserError $ PENotFound . (<> path)
        Just tr -> return $ fmap (path,) tr

inElemTrav
  :: (Monad m, Foldable g, DomTraversable f)
  => NameMatcher                -- ^ Name of tag to traverse in
  -> DomParserT Identity m a
  -> DomParserT g m (f a)
inElemTrav n = inFilteredTrav deeper
  where
    deeper = (DomPath [_nmShow n],) . toListOf (folded . nodes . folded . _Element . elMatchName n)

-- | Runs parser inside first children element with given name
inElem
  :: (Monad m, Foldable g)
  => NameMatcher
  -> DomParserT Identity m a
  -> DomParserT g m a
inElem n = fmap runIdentity . inElemTrav n

inElemAll
  :: (Monad m, Foldable g)
  => NameMatcher
  -> DomParserT Identity m a
  -> DomParserT g m [a]
inElemAll = inElemTrav

inElemMay
  :: (Monad m, Foldable g)
  => NameMatcher
  -> DomParserT Identity m a
  -> DomParserT g m (Maybe a)
inElemMay = inElemTrav

inElemNe
  :: (Monad m, Foldable g)
  => NameMatcher
  -> DomParserT Identity m a
  -> DomParserT g m (NonEmpty a)
inElemNe = inElemTrav

{- | Dive given parser's current tags set into the given path. The @divePath
["a", "b"]@ differs from @inElem "a" . inElem "b"@. Namely the first variant
will not fail if occured tag "a" which does not contains tag "b". This
behaviour is desireable when you dont want to parse whole XML and just want
to pull tags in some path. The other difference is in traversing inner
elements. Consider this code

@
inElem "a" $ inElem "b" $ inElemAll "c" fromDom
@

which translates to pseudo-CSS query like: @a:nth(1) > b:nth(1) > c > fromDom@

@
divePath ["a", "b"] $ inElemAll "c" fromDom
@

which translates like: @a > b > c > fromDom@

As you can see, inElem always takes first element and runs inner parser in this
single element, unlike 'divePath' which runs inner parser @in all@ descendants
in given path.
-}

divePath
  :: forall m g a
   . (Monad m, Foldable g)
  => [NameMatcher]
  -> DomParserT [] m a
  -> DomParserT g m a
divePath path = magnify $ to modElems
  where
    modElems
      = over pdElements (toListOf $ folded . diver)
      . over pdPath (<> DomPath (map _nmShow path))
    diver :: Fold Element Element
    diver    = foldr (.) id $ map toDive path
    toDive n = nodes . folded . _Element . elMatchName n

diveElem
  :: (Monad m, Foldable g)
  => NameMatcher
  -> DomParserT [] m a
  -> DomParserT g m a
diveElem p = divePath [p]

-- | Ignore arbitrary current element if it conforms to predicate.
ignoreElem
  :: (Monad m)
  => (Element -> Bool)
     -- ^ Predicate checking that we must ignore some current tag. If returns
     -- 'True' then parser will not be runned and combinator just returns Nothing.
  -> DomParserT Identity m a
  -> DomParserT Identity m (Maybe a)
ignoreElem test parser = do
  ign <- view $ pdElements . to (test . runIdentity)
  if ign then pure Nothing else Just <$> parser

-- | If current element has no children nodes does not run parser and returns
-- Nothing. Otherwise runs parser inside current element. Useful when you got
-- XML with strange empty elements which must be just ignored, but `inElem` runs
-- parser inside of this elements which causes to parser error.
ignoreEmpty
  :: (Monad m)
  => DomParserT Identity m a
  -> DomParserT Identity m (Maybe a)
ignoreEmpty = ignoreElem test
  where
    test e = null $ e ^. nodes

-- | If all current elements contains blank content, or contains nothing at all
-- , then returns Nothing, else runs parser.
ignoreBlank
  :: (Monad m)
  => DomParserT Identity m a
  -> DomParserT Identity m (Maybe a)
ignoreBlank = ignoreElem test
  where
    test e =
      let
        elems = e ^.. nodes . folded . _Element
        cont = mconcat $ e ^.. nodes . folded . _Content
      in if | not $ null elems      -> False
            | T.null $ T.strip cont -> True
            | otherwise             -> False

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
    let pinit = if null (unDomPath p) then [] else init $ unDomPath p
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
    | not $ null els -> Nothing
    | null conts     -> Nothing
    | otherwise      -> Just $ mconcat conts

-- | Parses content inside current tag. It expects current element set
-- consists of exactly ONE element. If current element does not
-- contains content or have other elements as childs then throws error
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
