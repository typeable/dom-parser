module Text.XML.DOM.Parser.Combinators
  ( -- * Generic combinators to traverse descendants
    traverseElems
  , inFilteredTrav
    -- * Using 'Buildable'
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
  ) where

import Control.Lens
import Control.Monad.Reader
import Data.Foldable as F
import Data.List as L
import Data.List.NonEmpty (NonEmpty(..))
import Data.Monoid
import Data.Text as T
import Data.Traversable
import Text.XML
import Text.XML.DOM.Parser.Buildable
import Text.XML.DOM.Parser.Types
import Text.XML.Lens


-- | Generic function to traverse arbitrary inner elements.
traverseElems
  :: (Monad m, Foldable g, Traversable f)
  => ([Element] -> DomParserT g m (f (DomPath, Element)))
     -- ^ Takes list of current elements and returns container with
     -- pairs of subpath (relatively to current elements) and element
     -- to run parser in
  -> DomParserT Identity m a
     -- ^ Parser to run for each element found in traversable 'f'
  -> DomParserT g m (f a)
traverseElems trav parser = do
  pd <- ask
  inner <- trav $ pd ^.. pdElements . folded
  for inner $ \(subpath, e) -> do
    let
      newpd = ParserData
        { _pdElements = Identity e
        , _pdPath     = pd ^. pdPath <> subpath }
    lift $ runReaderT parser newpd

-- | Traverses elements located in same path using filtering function
inFilteredTrav
  :: (Monad m, Foldable g, Buildable f)
  => ([Element] -> (DomPath, [Element]))
  -- ^ Takes list of current elements and returns some descendants
  -- subset and path this descendants located at. Path is should be
  -- same for all descendants and required for error message
  -> DomParserT Identity m a
  -> DomParserT g m (f a)
inFilteredTrav deeper = traverseElems trav
  where
    trav e = do
      let (path, elems) = deeper e
      case build elems of
        Nothing -> throwParserError $ PENotFound . (<> path)
        Just tr -> return $ fmap (path,) tr

-- | Runs parser arbitrary times, depending on 'Buildable' instance of
-- 'f'. For example if 'f' becomes 'NonEmpty' then 'inElemTrav' finds
-- @one or more@ elements matched by given 'ElemMatcher' and run
-- parser in each found element, then returns @NonEmpty a@ of results.
inElemTrav
  :: (Monad m, Foldable g, Buildable f)
  => ElemMatcher                -- ^ Tag(s) matcher to traverse in
  -> DomParserT Identity m a
  -> DomParserT g m (f a)
inElemTrav n = inFilteredTrav deeper
  where
    elemsFold = folded . nodes . folded . _Element . elMatch n
    deeper = (DomPath [_emShow n],) . toListOf elemsFold

-- | Runs parser inside first children element matched by macher
inElem
  :: (Monad m, Foldable g)
  => ElemMatcher
  -> DomParserT Identity m a
  -> DomParserT g m a
inElem n = fmap runIdentity . inElemTrav n

inElemAll
  :: (Monad m, Foldable g)
  => ElemMatcher
  -> DomParserT Identity m a
  -> DomParserT g m [a]
inElemAll = inElemTrav

inElemMay
  :: (Monad m, Foldable g)
  => ElemMatcher
  -> DomParserT Identity m a
  -> DomParserT g m (Maybe a)
inElemMay = inElemTrav

inElemNe
  :: (Monad m, Foldable g)
  => ElemMatcher
  -> DomParserT Identity m a
  -> DomParserT g m (NonEmpty a)
inElemNe = inElemTrav

{- | Dive given parser's current tags set into the given path. The @divePath
["a", "b"]@ differs from @inElem "a" $ inElem "b"@. Namely the first variant
will not fail if occured tag "a" which does not contains tag "b". This
behaviour is desireable when you dont want to parse whole XML and just want
to pull tags located in some path. The other difference is in traversing inner
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

Note also that 'divePath' takes parser parameterized by @[]@ not by
'Identity'. This because when you dive using some path you will get a
list of found elements and all these elements will be @current@ for
parser.
-}

divePath
  :: forall m g a
   . (Monad m, Foldable g)
  => [ElemMatcher]
  -> DomParserT [] m a
  -> DomParserT g m a
divePath path = magnify $ to modElems
  where
    modElems
      = over pdElements (toListOf $ folded . diver)
      . over pdPath (<> DomPath (L.map _emShow path))
    diver :: Fold Element Element
    diver    = F.foldr (.) id $ L.map toDive path
    toDive n = nodes . folded . _Element . elMatch n

diveElem
  :: (Monad m, Foldable g)
  => ElemMatcher
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
    test e = L.null $ e ^. nodes

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
      in if | not $ L.null elems    -> False
            | T.null $ T.strip cont -> True
            | otherwise             -> False
