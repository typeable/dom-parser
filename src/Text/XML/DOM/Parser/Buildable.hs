module Text.XML.DOM.Parser.Buildable
  ( Buildable(..)
  ) where

import Data.Functor.Identity
import Data.List.NonEmpty as NE
import Data.Maybe

-- | Class of traversable functors which may be constructed from list
class Traversable f => Buildable f where
  -- | If method return Nothing this means we can not build
  -- traversable from given list. In this case 'inFilteredTrav' should
  -- fail traversing.
  build :: [a] ->  Maybe (f a)

instance Buildable Identity where
  build = fmap Identity . listToMaybe

instance Buildable [] where
  build = Just

instance Buildable Maybe where
  build = Just . listToMaybe

instance Buildable NonEmpty where
  build = NE.nonEmpty
