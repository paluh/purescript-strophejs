module Chat.Main where

import Prelude
import Chat.App (Action(..), update, view)
import Chat.Connection (Action(..))
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import DOM (DOM)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Network.HTTP.Affjax (AJAX)
import Pux (start)
import Pux.Renderer.React (renderToDOM)
import Signal.Channel (CHANNEL, channel, subscribe)
import Strophe (HTTP, ServerUrl(ServerUrl), Status(Disconnected))

-- stanza ∷ ∀ eff. Eff eff {s1 ∷ Stanza, s2 ∷ Stanza}
-- stanza = runST (do
--   b ← iq empty
--   c b "bar" empty
--   s1 ← buildStanza b
--   c b "baz" empty
--   t b "TEST TET"
--   up b
--   up b
--   c b "foo" empty
--   s2 ← buildStanza b
--   pure {s1, s2})
-- 
-- main =
--   {s1, s2} ← stanza
--   log (toString s1)
--   log (toString s2)

main :: ∀ eff. Eff ( ajax ∷ AJAX, err ∷ EXCEPTION, http ∷ HTTP , channel ∷ CHANNEL , console ∷ CONSOLE, dom ∷ DOM | eff ) Unit
main = do
  statusChannel ← channel Disconnected
  messageChannel ← channel Nothing
  let
    state =
      { connection:
          { connection: Nothing
          , credentials: Nothing
          , messageChannel: messageChannel
          , serverUrl: ServerUrl "http://localhost:5280/http-bind/"
          , statusChannel: statusChannel
          , status: Disconnected
          }
      , loginForm:
          { jid: ""
          , password: ""
          }
      , log: Nil
      }
  let
    statusSignal = subscribe statusChannel
    messageSignal = subscribe messageChannel
  app <- start
    { initialState: state
    , foldp: update
    , view: view
    , inputs:
      [(ConnectionAction <<< StatusChange) <$> statusSignal,
       (ConnectionAction <<< Message) <$> messageSignal]
    }
  -- connect conn (Username "paluh@localhost") (Password "test") (onStatusChange channel)
  renderToDOM "#app" app.markup app.input
