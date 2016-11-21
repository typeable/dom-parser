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
    -- * Checking current element properties
  , checkCurrentName
    -- * Parsing arbitrary content
  , parseContent
  , readContent
  ) where

import           Control.Lens
import           Control.Monad.Except
import           Control.Monad.Reader
import           Data.List.NonEmpty (NonEmpty(..))
import           Data.Monoid
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Traversable
import           Data.Typeable
import           Text.Read
import           Text.Shakespeare.Text (st)
import           Text.XML
import           Text.XML.DOM.Parser.Types
import           Text.XML.Lens


-- | Generic function to traverse arbitrary inner cursors.
traverseElems
  :: (Monad m, Foldable g, Traversable f)
  => ([Element] -> DomParserT g m (f ([Text], Element)))
     -- ^ Takes set of current elements and
  -> DomParserT Identity m a
     -- ^ Parser will be runned for each element found in traversable
  -> DomParserT g m (f a)
traverseElems trav parser = do
  pd <- ask
  inner <- trav $ pd ^.. pdElements . folded
  for inner $ \(subpath, e) -> do
    let newpd = ParserData
          { _pdElements = Identity e
          , _pdPath     = pd ^. pdPath <> subpath }
    magnify (to $ const newpd) parser

-- | Takes function filtering
inFilteredTrav
  :: (Monad m, Foldable g, DomTraversable f)
  => ([Element] -> ([Text], [Element]))
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
  => Text
  -> DomParserT Identity m a
  -> DomParserT g m (f a)
inElemTrav n = inFilteredTrav deeper
  where
    deeper = ([n],) . toListOf (folded . nodes . folded . _Element . ell n)

-- | Runs parser inside first children element with given name
inElem
  :: (Monad m, Foldable g)
  => Text
  -> DomParserT Identity m a
  -> DomParserT g m a
inElem n = fmap runIdentity . inElemTrav n

inElemAll
  :: (Monad m, Foldable g)
  => Text
  -> DomParserT Identity m a
  -> DomParserT g m [a]
inElemAll = inElemTrav

inElemMay
  :: (Monad m, Foldable g)
  => Text
  -> DomParserT Identity m a
  -> DomParserT g m (Maybe a)
inElemMay = inElemTrav

inElemNe
  :: (Monad m, Foldable g)
  => Text
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
  => [Text]
  -> DomParserT [] m a
  -> DomParserT g m a
divePath path = magnify $ to modElems
  where
    modElems
      = over pdElements (toListOf $ folded . diver)
      . over pdPath (<> path)
    diver :: Fold Element Element
    diver    = foldr (.) id $ map toDive path
    toDive n = nodes . folded . _Element . ell n

diveElem
  :: (Monad m, Foldable g)
  => Text
  -> DomParserT [] m a
  -> DomParserT g m a
diveElem p = divePath [p]

-- | Ignore arbitrary current element if it conforms to predicate.
ignoreElem
  :: (Monad m)
  => (Element -> Bool)
     -- ^ Predicate checking that we must ignore some current tag. If returns
     -- true then parser will not be runned and combinator just returns Nothing.
  -> DomParserT Identity m a
  -> DomParserT Identity m (Maybe a)
ignoreElem test parser = do
  ign <- view $ pdElements . to (test . runIdentity)
  if ign then pure Nothing else Just <$> parser

-- | If current element has no children nodes does not run parser and returns
-- Nothing. Otherwise runs parser inside current element. Usefull when you got
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

-- | If name of current tag differs from first argument throws 'PENotFound' with
-- tag name replaced in last path's segment. Usefull for checking root
-- document's element name.
checkCurrentName
  :: (Monad m)
  => Text
  -> DomParserT Identity m ()
checkCurrentName n = do
  cn <- view $ pdElements . to runIdentity . localName
  unless (cn == n) $ do
    p <- view pdPath
    let pinit = if null p then [] else init p
    throwError $ ParserErrors [PENotFound $ pinit ++ [n]]
  return ()

-- | Parses content inside current tag. It expects current element set consists
-- of exactly ONE element. Throws error if current elements set contains
-- multiple of them.
parseContent
  :: (Monad m)
  => (Text -> DomParserT Identity m a)
  -> DomParserT Identity m a
parseContent parse = do
  e <- view $ pdElements . to runIdentity
  let
    nds = e ^. nodes
    els = nds ^.. folded . _Element
    conts = nds ^.. folded . _Content
  when (not $ null els) $ throwParserError PEContentNotFound
  when (null conts) $ throwParserError PEContentNotFound
  parse $ mconcat conts

readContent
  :: forall m g a
   . (Read a, Typeable a, Monad m)
  => Text
  -> DomParserT g m a
readContent t = case readMaybe $ T.unpack t of
  Nothing -> throwParserError $ PEWrongFormat [st|Not readable #{n}: #{t}|]
  Just a  -> pure a
  where
    n = show $ typeRep (Proxy :: Proxy a)
