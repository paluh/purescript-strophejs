module Chat.Connection where

import Prelude
import Strophe as Strophe
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Debug.Trace (traceAnyA)
import Pux (EffModel, noEffects, onlyEffects)
import Signal.Channel (CHANNEL, Channel, send)
import Strophe (Connection, HTTP, Jid, Password, ServerUrl, Stanza, Status(Disconnected), disconnect, toString)

type State =
  { connection ∷ Maybe Connection
  , serverUrl ∷ ServerUrl
  , credentials ∷
      Maybe
        { jid ∷ Jid
        , password ∷ Password
        }
  , messageChannel ∷ Channel (Maybe Stanza)
  , statusChannel ∷ Channel Status
  , status ∷ Status
  }

data Action
  = StatusChange Status
  | Message (Maybe Stanza)
  | Connect Jid Password
  -- to satisfy typechecker
  | Disconnect
  | NewConnectionSpawned Connection

update ∷ ∀ eff. Action → State → EffModel State Action (http ∷ HTTP | eff)
update (Message (Just stanza)) state =
  onlyEffects state [ traceAnyA (toString stanza) >>= const (traceAnyA "ABOVE IS STANZA") >>= const (pure Nothing) ]
update (StatusChange Disconnected) state =
  noEffects $ state { connection = Nothing, status = Disconnected}
update (StatusChange status) state = noEffects $ state { status = status }
update Disconnect state@{ connection: Just conn } =
  { state: state { connection = Nothing }
  , effects: [liftEff (disconnect conn "on request") >>= const (pure Nothing)]
  }
update (Connect jid password) state@{ connection, messageChannel, statusChannel } =
  { state: state { credentials = Just { jid, password }, connection = Nothing }
  , effects: [ spawnConnection ]
  }
 where
  spawnConnection = liftEff $ do
    (flip disconnect "we are going to create new connection") `traverse` connection
    conn ← Strophe.connection state.serverUrl
    Strophe.addHandler conn (\stanza → send messageChannel (Just stanza) >>= const (pure true))
    connect conn statusChannel jid password
    pure <<< Just $ NewConnectionSpawned conn
update (NewConnectionSpawned conn) state = noEffects $ state { connection = Just conn }
-- debug log
update a s = onlyEffects s [liftEff $ (traceAnyA a >>= const (traceAnyA s) >>= const (pure Nothing))]

connect ∷ ∀ eff. Connection → Channel Status → Jid → Password → Eff ( http ∷ HTTP , channel ∷ CHANNEL | eff ) Unit
connect connection statusChannel jid password = do
  Strophe.connect connection jid password onStatusChange
 where
  onStatusChange status = send statusChannel status

