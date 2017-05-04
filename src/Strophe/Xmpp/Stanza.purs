module Strophe.Xmpp.Stanza where

import Prelude
import Control.Error.Util (hoistMaybe)
import Control.Monad.Eff (Eff)
import Control.Monad.Maybe.Trans (MaybeT(..), runMaybeT)
import DOM (DOM)
import DOM.Node.Document (documentElement)
import DOM.Node.Element (getAttribute, tagName)
import DOM.Node.Types (Document, Element)
import Data.Array (elem)
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Nullable (toMaybe)
import Strophe (StanzaDocument)
import Strophe.Xmpp.Jid (Jid)
import Unsafe.Coerce (unsafeCoerce)

-- Code highly inspired by haskell pontarius-xmpp

data Stanza
  = IqRequest     IqRequest
  | IqResult      IqResult
  | OtherStanza   StanzaDocument
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
  , payload ∷ Maybe Element
  -- , attributes ∷ [ExtendedAttribute]
  }

asDocument ∷ StanzaDocument → Document
asDocument = unsafeCoerce <<< unwrap

parseIqRequestType ∷ String → Maybe IqRequestType
parseIqRequestType s
  | s == "get" = Just Get
  | s == "set" = Just Set
  | otherwise = Nothing

fromDocument ∷ ∀ eff. StanzaDocument → Eff (dom ∷ DOM | eff) (Maybe Stanza)
fromDocument stanzaDocument = runMaybeT ( do
  root ← MaybeT $ toMaybe <$> documentElement (asDocument stanzaDocument)
  case tagName root of
    "iq" → do
      iqType ← MaybeT $ toMaybe <$> getAttribute "type" root
      id ← MaybeT $ toMaybe <$> getAttribute "id" root
      to ← MaybeT $ (Just <<< toMaybe) <$> getAttribute "to" root
      from ← MaybeT $ (Just <<< toMaybe) <$> getAttribute "from" root
      if iqType `elem` ["get", "set"]
        then do
          iqType' ← hoistMaybe (parseIqRequestType iqType)
          pure $ IqRequest { requestType: iqType', id, to: Nothing, from: Nothing}
        -- XXX: Fix parsing
        else pure $ OtherStanza stanzaDocument
    otherwise → pure (OtherStanza stanzaDocument))


-- type UnidentifiedIqRequest = IqRequestBase ()
-- type IqRequest = IqRequestBase (id ∷ String)
