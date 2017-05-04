module Chat.Connection where

import Prelude
import Signal.Channel as Channel
import Strophe as Strophe
import Chat.Utils (onlyEffEffect', onlyEffect')
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Now (NOW, nowDateTime)
import Data.DateTime.Locale (LocalDateTime)
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse)
import Debug.Trace (traceAnyA)
import Pux (EffModel, noEffects)
import Signal.Channel (CHANNEL, Channel)
import Strophe (CONNECTION, Connection, HTTP, Jid, Password, ServerUrl, StanzaDocument, Status(Disconnected), disconnect, toString)

type State =
  { connection ∷ Maybe Connection
  , serverUrl ∷ ServerUrl
  , credentials ∷
      Maybe
        { jid ∷ Jid
        , password ∷ Password
        }
  , incomingStanzaChannel ∷ Channel (Maybe StanzaDocument)
  , outgoingStanzaChannel ∷
      Channel
        (Maybe
          { stanza ∷ StanzaDocument
          , sentAt ∷ LocalDateTime
          }
        )
  , statusChannel ∷ Channel Status
  , status ∷ Status
  }

data Action
  = StatusChange Status
  | StanzaSent (Maybe {stanza ∷ StanzaDocument, sentAt ∷ LocalDateTime})
  | StanzaReceived (Maybe StanzaDocument)
  | Connect Jid Password
  -- to satisfy typechecker
  | Disconnect
  | NewConnectionSpawned Connection

-- handling communication
update ∷ ∀ eff. Action → State → EffModel State Action (connection ∷ CONNECTION, http ∷ HTTP | eff)
update (StanzaReceived (Just stanza)) state =
  onlyEffect' state (traceAnyA (toString stanza) >>= const (traceAnyA "ABOVE IS STANZA"))

-- handling connection
update (StatusChange Disconnected) state =
  noEffects $ state { connection = Nothing, status = Disconnected}
update (StatusChange status) state = noEffects $ state { status = status }
update Disconnect state@{ connection: Just conn } =
  { state: state { connection = Nothing }
  , effects: [liftEff (disconnect conn "on request") >>= const (pure Nothing)]
  }
update (Connect jid password) state@{ connection, incomingStanzaChannel, statusChannel } =
  { state: state { credentials = Just { jid, password }, connection = Nothing }
  , effects: [ spawnConnection ]
  }
 where
  spawnConnection = liftEff $ do
    (flip disconnect "we are going to create new connection") `traverse` connection
    conn ← Strophe.connection state.serverUrl
    Strophe.addHandler
      conn
      (\stanza → Channel.send incomingStanzaChannel (Just stanza) >>= const (pure true))
    connect conn jid password statusChannel
    pure <<< Just $ NewConnectionSpawned conn
update (NewConnectionSpawned conn) state = noEffects $ state { connection = Just conn }
-- debug log
update a s = onlyEffEffect' s (traceAnyA a >>= const (traceAnyA s))

connect ∷ ∀ eff. Connection → Jid → Password → Channel Status → Eff ( http ∷ HTTP , channel ∷ CHANNEL, connection ∷ CONNECTION | eff ) Unit
connect connection jid password statusChannel = do
  Strophe.connect connection jid password onStatusChange
 where
  onStatusChange status = Channel.send statusChannel status

send ∷ ∀ eff. Connection → StanzaDocument → Channel (Maybe {stanza ∷ StanzaDocument, sentAt ∷ LocalDateTime}) → Eff (http ∷ HTTP, connection ∷ CONNECTION, channel ∷ CHANNEL, now ∷ NOW | eff) Strophe.StanzaId
send connection stanza outgoingStanzaChannel = do
  stanzaId ← Strophe.send' connection stanza
  sentAt ← nowDateTime
  Channel.send outgoingStanzaChannel (Just {stanza, sentAt})
  pure stanzaId
