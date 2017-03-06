module Strophe
  ( attached
  , authenticating
  , authfail
  , connect
  , connected
  , connecting
  , connection
  , Connection
  , connfail
  , conntimeout
  , disconnect
  , disconnected
  , disconnecting
  , error
  , HTTP
  , Password(..)
  , ServerUrl(..)
  , showStatus
  , Status
  , Jid(..)
  )
 where

import Prelude
import Control.Monad.Eff (Eff)
import Data.Function.Uncurried (Fn1, runFn1)
import Data.Function.Eff (EffFn1, EffFn2, EffFn4, runEffFn2, runEffFn4, mkEffFn1)
import Data.Generic.Rep (class Generic)
import Data.Generic.Rep.Show (genericShow)

foreign import data HTTP :: !
foreign import data Connection :: *

newtype Status = Status Int
derive instance eqStatus :: Eq Status
derive instance genericStatus :: Generic Status _
instance showStatusGeneric :: Show Status where
  show = genericShow

foreign import attached :: Status
foreign import authenticating :: Status
foreign import authfail :: Status
foreign import connected :: Status
foreign import connecting :: Status
foreign import connfail :: Status
foreign import conntimeout :: Status
foreign import disconnected :: Status
foreign import disconnecting :: Status
foreign import error :: Status

showStatus ∷ Status → String
showStatus s
  | s == attached = "attached"
  | s == authenticating = "authenticating"
  | s == authfail = "authfail"
  | s == connected = "connected"
  | s == connecting = "connecting"
  | s == connfail = "connfail"
  | s == conntimeout = "conntimeout"
  | s == disconnected = "disconnected"
  | s == disconnecting = "disconnecting"
  | s == error = "error"
  | otherwise = "unknown status : " <> show s

newtype ServerUrl = ServerUrl String
newtype Jid = Jid String
newtype Password = Password String

foreign import connectionImpl ::
  forall eff. Fn1 ServerUrl (Eff (http :: HTTP | eff) Connection)

connection :: forall eff. ServerUrl -> Eff (http :: HTTP | eff) Connection
connection = runFn1 connectionImpl

foreign import connectImpl ::
  forall eff.
    EffFn4 (http :: HTTP | eff)
      Connection
      Jid
      Password
      (EffFn1 (http ∷ HTTP | eff) Status Unit)
      Unit

connect ::
  forall eff.
    Connection ->
    Jid ->
    Password ->
    (Status -> Eff (http ∷ HTTP | eff) Unit) ->
    (Eff (http ∷ HTTP | eff) Unit)
connect conn user pass callback = runEffFn4 connectImpl conn user pass (mkEffFn1 callback)

foreign import disconnectImpl ∷
  forall eff.
    EffFn2 (http :: HTTP | eff)
      Connection
      String
      Unit

disconnect ::
  forall eff.
    Connection ->
    String →
    (Eff (http :: HTTP | eff) Unit)
disconnect conn reason = runEffFn2 disconnectImpl conn reason
