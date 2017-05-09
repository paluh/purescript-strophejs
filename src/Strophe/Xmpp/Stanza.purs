module Strophe.Xmpp.Stanza where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import Control.Monad.Trans.Class (lift)
import DOM (DOM)
import DOM.Node.Element (getAttribute, tagName)
import DOM.Node.Types (Element)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Strophe (StanzaDocument)
import Strophe.Xmpp.Jid (Jid)
import Unsafe.Coerce (unsafeCoerce)

-- Code highly inspired by haskell pontarius-xmpp

data Stanza
  = IqRequest IqRequest
  | IqResult IqResult
  | IqError IqError
  | OtherStanza StanzaDocument
  -- | IQErrorS       !IQError
  -- | MessageS       !Message
  -- | MessageErrorS  !MessageError
  -- | PresenceS      !Presence
  -- | PresenceErrorS !PresenceError
-- deriving (Eq, Show)

data IqRequestType = Get | Set
derive instance genericIqRequestType ∷ Generic IqRequestType _
derive instance eqIqRequestType ∷ Eq IqRequestType
derive instance ordIqRequestType ∷ Ord IqRequestType
data IqResultType = Error | Result
derive instance genericIqResultType ∷ Generic IqResultType _
derive instance eqIqResultType ∷ Eq IqResultType
derive instance ordIqResultType ∷ Ord IqResultType
data IqType
  = IqResultType IqResultType
  | IqRequestType IqRequestType

derive instance genericIqType ∷ Generic IqType _
derive instance eqIqType ∷ Eq IqType
derive instance ordIqType ∷ Ord IqType

type IqRequest =
  { id ∷ String
  , from ∷ Maybe Jid
  , to ∷ Maybe Jid
  -- , langTag ∷ Maybe LangTag
  , requestType ∷ IqRequestType
  -- , payload ∷ Element
  }

type IqResult =
  { id  ∷ String
  , from ∷ Maybe Jid
  , to ∷ Maybe Jid
  -- , langTag ∷ Maybe LangTag
  -- , payload ∷ Maybe Element
  -- , attributes ∷ [ExtendedAttribute]
  }

type IqError =
  { id          :: String
  , from        :: (Maybe Jid)
  , to          :: (Maybe Jid)
  -- , langTag     :: !(Maybe LangTag)
  -- , stanzaError :: !StanzaError
  -- , payload     :: (Maybe Element) -- should this be []?
  -- , attributes  :: ![ExtendedAttribute]
  }

asDocument ∷ StanzaDocument → Element
asDocument = unsafeCoerce <<< unwrap

parseIqType ∷ String → Maybe IqType
parseIqType s
  | s == "get" = Just (IqRequestType Get)
  | s == "set" = Just (IqRequestType Set)
  | s == "error" = Just (IqResultType Error)
  | s == "result" = Just (IqResultType Result)
  | otherwise = Nothing

fromDocument ∷ ∀ eff. StanzaDocument → Eff (dom ∷ DOM | eff) (Maybe Stanza)
fromDocument stanzaDocument = runMaybeT ( do
  let root = asDocument stanzaDocument
  case tagName root of
    "iq" → do
      iqType ← MaybeT $ ((_ >>= parseIqType)) <$> getAttribute "type" root
      id ← MaybeT $ getAttribute "id" root
      to ← lift $ getAttribute "to" root
      from ← lift $ getAttribute "from" root
      pure $ case iqType of
        (IqRequestType requestType) → IqRequest { requestType, id, to, from }
        (IqResultType Error) → IqError {id, to, from}
        (IqResultType Result) → IqResult {id, to, from}
    otherwise → pure (OtherStanza stanzaDocument))

