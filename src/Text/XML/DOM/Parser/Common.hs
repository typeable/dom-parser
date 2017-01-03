module Text.XML.DOM.Parser.Common where

import Data.Monoid
import Data.Text as T
import Data.Typeable
import Text.Read


-- | @since 1.0.0
readBool :: Text -> Either Text Bool
readBool t =
  let
    lowt  = T.toLower $ T.strip t
    tvals = ["y", "yes", "t", "true", "1"]
    fvals = ["n", "no", "f", "false", "0"]
  in if
    | lowt `elem` tvals -> Right True
    | lowt `elem` fvals -> Right False
    | otherwise         ->
        Left $ "Could not read " <> t <> " as Bool"

-- | Expects text to be single character
--
-- @since 1.0.0
readChar :: Text -> Either Text Char
readChar t = case T.unpack $ T.strip t of
  [c] -> Right c
  _   -> Left "Should have exactly one non-blank character"

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
