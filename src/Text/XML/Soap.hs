module Text.XML.Soap where

import Control.Lens
import Data.String
import Text.XML
import Text.XML.DOM.Parser
import Text.XML.Writer

data XmlEnvelope h b = XmlEnvelope
  { _xeHeader :: !(Maybe h)
  , _xeBody   :: !(Maybe b)
  } deriving (Eq, Show)

makeLenses ''XmlEnvelope

xmlEnvelopeToDocument :: (ToXML h, ToXML b) => XmlEnvelope h b -> Document
xmlEnvelopeToDocument xe = doc
  where
    sname n = fromString $ "{http://schemas.xmlsoap.org/soap/envelope/}" <> n
    doc    =
      Document
        { documentPrologue = Prologue [] Nothing []
        , documentRoot     =
          Element
            { elementName       = sname "Envelope"
            , elementAttributes = mempty
            , elementNodes      = [NodeElement header, NodeElement body]
            }
        , documentEpilogue = []
        }
    header =
      Element
        { elementName       = sname "Header"
        , elementAttributes = mempty
        , elementNodes      = render . toXML $ xe ^. xeHeader
        }
    body   =
      Element
        { elementName       = sname "Body"
        , elementAttributes = mempty
        , elementNodes      = render . toXML $ xe ^. xeBody
        }

instance (FromDom h, FromDom b) => FromDom (XmlEnvelope h b) where
  fromDom = XmlEnvelope <$> inElemMay "Header" fromDom
                        <*> inElemMay "Body" fromDom
