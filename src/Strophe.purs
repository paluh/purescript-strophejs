module Strophe
  ( addHandler
  , buildStanza
  , Connection
  , c
  , connection
  , connect
  , connect'
  , disconnect
  , HTTP
  , iq
  , Jid(..)
  , Password(..)
  , send
  , ServerUrl(..)
  , Stanza
  , Status(..)
  , STBuilder
  , t
  , toString
  , up
  )
 where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.ST (ST)
import Data.Function.Eff (EffFn1, EffFn2, EffFn3, EffFn5, mkEffFn1,
                          runEffFn1, runEffFn2, runEffFn3, runEffFn5)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)
import Data.StrMap (StrMap)
import Partial.Unsafe (unsafePartial)

foreign import data HTTP ∷ !
foreign import data Connection ∷ *
foreign import data StropheStanza ∷ *

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
derive instance genericStatus ∷ Generic Status _
derive instance eqStatus ∷ Eq Status
instance showStatus ∷ Show Status where show = genericShow

-- values used to implement convertion
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

_toStatus ∷ Partial ⇒ Int → Status
_toStatus s | s == attached = Attached
            | s == authenticating = Authenticating
            | s == authfail = Authfail
            | s == connected = Connected
            | s == connecting = Connecting
            | s == connfail = Connfail
            | s == conntimeout = Conntimeout
            | s == disconnected = Disconnected
            | s == disconnecting = Disconnecting
            | s == error = Error

newtype ServerUrl = ServerUrl String
newtype Jid = Jid String
newtype Password = Password String

foreign import connectionImpl ∷
  ∀ eff. Fn1 ServerUrl (Eff (http ∷ HTTP | eff) Connection)
connection ∷ ∀ eff. ServerUrl → Eff (http ∷ HTTP | eff) Connection
connection = runFn1 connectionImpl

foreign import connectImpl ∷
  ∀ eff.
    EffFn5 (http ∷ HTTP | eff)
      (Int → Status)
      Connection
      Jid
      Password
      (EffFn1 (http ∷ HTTP | eff) Status Unit)
      Unit
connect ∷
  ∀ eff.
    Connection →
    Jid →
    Password →
    (Status → Eff (http ∷ HTTP | eff) Unit) →
    (Eff (http ∷ HTTP | eff) Unit)
connect conn user pass callback = runEffFn5 connectImpl (unsafePartial _toStatus) conn user pass (mkEffFn1 callback)

connect' ∷ 
  ∀ eff.
    Connection →
    Jid →
    Password →
    ({status ∷ Status, connection ∷ Connection} → Eff (http ∷ HTTP | eff) Unit) →
    (Eff (http ∷ HTTP | eff) Unit)
connect' conn user pass callback = runEffFn5 connectImpl (unsafePartial _toStatus) conn user pass (mkEffFn1 (\status → callback { status, connection: conn }))

foreign import disconnectImpl ∷
  ∀ eff.
    EffFn2 (http ∷ HTTP | eff)
      Connection
      String
      Unit
disconnect ∷
  ∀ eff.
    Connection →
    String →
    Eff (http ∷ HTTP | eff) Unit
disconnect conn reason = runEffFn2 disconnectImpl conn reason

-- | Fully imperative interface to strphejs Builder.

foreign import data STBuilder ∷ * → *
foreign import data Stanza ∷ *

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

foreign import upImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (STBuilder h) Unit
up ∷ ∀ h r. STBuilder h → Eff ( st ∷ ST h | r ) Unit
up = runEffFn1 upImpl

foreign import buildStanzaImpl ∷ ∀ h r. EffFn1 (st ∷ ST h | r) (STBuilder h) Stanza
buildStanza ∷ ∀ h r. STBuilder h → Eff ( st ∷ ST h | r ) Stanza
buildStanza = runEffFn1 buildStanzaImpl

foreign import toString ∷ Stanza → String

foreign import sendImpl ∷
  ∀ eff.
    EffFn2 (http ∷ HTTP | eff)
      Connection
      Stanza
      Unit
send ∷ ∀ eff. Connection → Stanza → Eff (http ∷ HTTP | eff) Unit
send = runEffFn2 sendImpl

foreign import addHandlerImpl ∷
  ∀ eff.
    EffFn2 (http ∷ HTTP | eff)
      Connection
      (EffFn1 (http ∷ HTTP | eff) Stanza Boolean)
      Unit
addHandler ∷ ∀ eff. Connection → (Stanza → Eff (http ∷ HTTP | eff) Boolean) → Eff (http ∷ HTTP | eff) Unit
addHandler conn handler = runEffFn2 addHandlerImpl conn (mkEffFn1 handler)

