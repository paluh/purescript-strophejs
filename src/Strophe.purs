module Strophe
  ( addHandler
  , build
  , Connection
  , CONNECTION
  , c
  , connection
  , connect
  , connect'
  , deleteHandler
  , disconnect
  , HTTP
  , iq
  , Jid(..)
  , Password(..)
  , send
  , send'
  , ServerUrl(..)
  , StanzaDocument
  , StanzaHandlerRef
  , StanzaId
  , Status(..)
  , STBuilder
  , t
  , toString
  , up
  )
 where

import Prelude
import Control.Monad.Eff (Eff, runPure, kind Effect)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, EffFn3, EffFn5, mkEffFn1, runEffFn1, runEffFn2, runEffFn3, runEffFn5)
import Control.Monad.ST (ST, pureST)
import DOM.Node.Types (Document)
import Data.Array (replicate, (!!))
import Data.Array.ST (pokeSTArray, runSTArray, thaw)
import Data.Foldable (maximum, sequence_)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.Maybe (Maybe(..), fromJust, fromMaybe)
import Data.Newtype (class Newtype, unwrap)
import Data.Nullable (Nullable, toNullable)
import Data.Profunctor.Strong (second)
import Data.StrMap (StrMap, fromFoldable)
import Data.Tuple (Tuple(Tuple), fst, uncurry)
import Partial.Unsafe (unsafePartial)

foreign import data HTTP ∷ Effect
-- CONNECTION is an effect which changes
-- connection instance state
-- for example getUniqueId changes state
-- of connection object without HTTP requests
foreign import data CONNECTION ∷ Effect

foreign import data Connection ∷ Type

data Status
  = Attached
  | Authenticating
  | Authfail
  | Connected
  | Connecting
  | Connfail
  | Conntimeout
  | Disconnected
  | Disconnecting
  | Error
  | Redirect
derive instance genericStatus ∷ Generic Status _
derive instance eqStatus ∷ Eq Status
instance showStatus ∷ Show Status where show = genericShow

foreign import attached ∷ Int
foreign import authenticating ∷ Int
foreign import authfail ∷ Int
foreign import connected ∷ Int
foreign import connecting ∷ Int
foreign import connfail ∷ Int
foreign import conntimeout ∷ Int
foreign import disconnected ∷ Int
foreign import disconnecting ∷ Int
foreign import error ∷ Int
foreign import redirect ∷ Int

_statusCache ∷ Array (Maybe Status)
_statusCache = runPure $ runSTArray (do
  let
    s =
      [ Tuple attached Attached
      , Tuple authenticating Authenticating
      , Tuple authfail Authfail
      , Tuple connected Connected
      , Tuple connecting Connecting
      , Tuple connfail Connfail
      , Tuple conntimeout Conntimeout
      , Tuple disconnected Disconnected
      , Tuple disconnecting Disconnecting
      , Tuple error Error
      , Tuple redirect Redirect
      ]
    m = fromMaybe attached (maximum (map fst s))
  arr ← thaw <<< replicate (m + 1) $ Nothing
  sequence_ $ map (uncurry (pokeSTArray arr) <<< second Just) s
  pure arr)

_toStatus :: Partial ⇒ Int → Status
_toStatus i = fromJust $ _statusCache !! i >>= id

newtype ServerUrl = ServerUrl String
derive instance newtypeServerUrl ∷ Newtype ServerUrl _

newtype Jid = Jid String
derive instance newtypeJid ∷ Newtype Jid _

newtype Password = Password String
derive instance newtypePassword ∷ Newtype Password _

foreign import connectionImpl ∷
  ∀ eff. ServerUrl → (Eff (http ∷ HTTP, connection :: CONNECTION | eff) Connection)
connection ∷ ∀ eff. ServerUrl → Eff (http ∷ HTTP, connection :: CONNECTION | eff) Connection
connection = connectionImpl

foreign import connectImpl ∷
  ∀ eff.
    EffFn5 (http ∷ HTTP, connection :: CONNECTION | eff)
      (Int → Status)
      Connection
      Jid
      Password
      (EffFn1 (http ∷ HTTP, connection :: CONNECTION | eff) Status Unit)
      Unit
connect ∷
  ∀ eff.
    Connection →
    Jid →
    Password →
    (Status → Eff (http ∷ HTTP, connection :: CONNECTION | eff) Unit) →
    (Eff (http ∷ HTTP, connection :: CONNECTION | eff) Unit)
connect conn user pass callback = runEffFn5 connectImpl (unsafePartial _toStatus) conn user pass (mkEffFn1 callback)

connect' ∷
  ∀ eff.
    Connection →
    Jid →
    Password →
    ({status ∷ Status, connection ∷ Connection} → Eff (http ∷ HTTP, connection :: CONNECTION | eff) Unit) →
    (Eff (http ∷ HTTP, connection :: CONNECTION | eff) Unit)
connect' conn user pass callback = runEffFn5 connectImpl (unsafePartial _toStatus) conn user pass (mkEffFn1 (\status → callback { status, connection: conn }))

foreign import disconnectImpl ∷
  ∀ eff.
    EffFn2 (http ∷ HTTP, connection :: CONNECTION | eff)
      Connection
      String
      Unit
disconnect ∷
  ∀ eff.
    Connection →
    String →
    Eff (http ∷ HTTP, connection :: CONNECTION | eff) Unit
disconnect conn reason = runEffFn2 disconnectImpl conn reason

-- | Fully imperative interface to strphejs Builder.

foreign import data STBuilder ∷ Type → Type
newtype StanzaDocument = StanzaDocument Document
derive instance newtypeStanzaDocument ∷ Newtype StanzaDocument _

foreign import iqImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (StrMap String) (STBuilder h)
iq ∷ ∀ h r. StrMap String → Eff (st ∷ ST h | r) (STBuilder h)
iq = runEffFn1 iqImpl

foreign import msgImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (StrMap String) (STBuilder h)
msg ∷ ∀ h r. StrMap String → Eff (st ∷ ST h | r) (STBuilder h)
msg = runEffFn1 msgImpl

foreign import presImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (StrMap String) (STBuilder h)
pres ∷ ∀ h r. StrMap String → Eff (st ∷ ST h | r) (STBuilder h)
pres = runEffFn1 presImpl

foreign import cImpl ∷ ∀ h r. EffFn3 (st ∷ ST h | r) (STBuilder h) String (StrMap String) Unit
c ∷ ∀ h r. STBuilder h → String → StrMap String → Eff (st ∷ ST h | r) Unit
c = runEffFn3 cImpl

foreign import tImpl ∷ ∀ h r. EffFn2 (st ∷ ST h | r) (STBuilder h) String Unit
t ∷ ∀ h r. STBuilder h → String → Eff (st ∷ ST h | r) Unit
t = runEffFn2 tImpl

-- add or remove attrs
foreign import attrsImpl ∷ ∀ h r. EffFn2 (st ∷ ST h | r) (STBuilder h) (StrMap (Nullable String)) Unit
attrs ∷ ∀ h r. STBuilder h → StrMap (Maybe String) → Eff (st ∷ ST h | r) Unit
attrs builder = runEffFn2 attrsImpl builder <<< map toNullable

foreign import upImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (STBuilder h) Unit
up ∷ ∀ h r. STBuilder h → Eff ( st ∷ ST h | r ) Unit
up = runEffFn1 upImpl

foreign import buildImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (STBuilder h) Document
build ∷ ∀ h r. STBuilder h → Eff ( st ∷ ST h | r ) StanzaDocument
build = (StanzaDocument <$> _) <<< runEffFn1 buildImpl

foreign import fromStanzaDocumentImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) Document (STBuilder h)
-- | Creates builder from stanza which points on the root of the tree
fromStanzaDocument ∷ ∀ h r. StanzaDocument → Eff ( st ∷ ST h | r ) (STBuilder h)
fromStanzaDocument = runEffFn1 fromStanzaDocumentImpl <<< unwrap

newtype StanzaId = StanzaId String
derive instance newtypeStanzaId ∷ Newtype StanzaId _

foreign import getUniqueIdImpl ∷
  ∀ eff. EffFn2 (connection ∷ CONNECTION | eff) Connection (Nullable String) String
getUniqueId ∷
  ∀ eff. Connection → Maybe String → Eff (connection ∷ CONNECTION | eff) StanzaId
getUniqueId conn suffix = StanzaId <$> runEffFn2 getUniqueIdImpl conn (toNullable suffix)

foreign import toString ∷ StanzaDocument → String

foreign import sendImpl ∷
  ∀ eff.
    EffFn2 (http ∷ HTTP, connection :: CONNECTION | eff)
      Connection
      StanzaDocument
      Unit
send ∷ ∀ eff. Connection → StanzaDocument → Eff (http ∷ HTTP, connection :: CONNECTION | eff) Unit
send = runEffFn2 sendImpl

-- send and set uniqueId for stanza
send' ∷ ∀ eff. Connection → StanzaDocument → Eff (http ∷ HTTP, connection :: CONNECTION | eff) StanzaDocument
send' conn stanza = do
  stanzaId ← getUniqueId conn Nothing
  let
    stanza' = pureST (do
      builder ← fromStanzaDocument stanza
      attrs builder (fromFoldable [Tuple "id" (Just <<< unwrap $ stanzaId)])
      build builder)
  send conn stanza'
  pure stanza'

foreign import data StanzaHandlerRef ∷ Type

foreign import addHandlerImpl ∷
  ∀ eff.
    EffFn2 (http ∷ HTTP, connection :: CONNECTION | eff)
      Connection
      (EffFn1 (http ∷ HTTP, connection :: CONNECTION | eff) StanzaDocument Boolean)
      StanzaHandlerRef
addHandler ∷
  ∀ eff.
    Connection →
    (StanzaDocument → Eff (http ∷ HTTP, connection :: CONNECTION | eff) Boolean) →
    Eff (http ∷ HTTP, connection :: CONNECTION | eff) StanzaHandlerRef
addHandler conn handler = runEffFn2 addHandlerImpl conn (mkEffFn1 handler)

foreign import deleteHandlerImpl ∷
  ∀ eff.
    EffFn2 (connection :: CONNECTION | eff)
      Connection
      StanzaHandlerRef
      Unit
deleteHandler ∷
  ∀ eff.
    Connection →
    StanzaHandlerRef →
    Eff (connection :: CONNECTION | eff) Unit
deleteHandler = runEffFn2 deleteHandlerImpl
