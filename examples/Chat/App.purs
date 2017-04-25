module Chat.App where

import Prelude
import Chat.Connection as Connection
import Chat.LoginForm as LoginForm
import Data.Either as Either
import Data.Lens as Lens
import Strophe as Strophe
import Chat.LoginForm (AuthenticationStatus(..))
import Chat.Utils (UpdateFun(..), focus, focusEffModel, runUpdateFun)
import Control.Monad.Aff.Console (CONSOLE)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.ST (runST)
import DOM (DOM)
import DOM.HTML.HTMLTrackElement.ReadyState (ReadyState(..))
import Data.Foldable (for_)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Monoid (mempty)
import Data.StrMap (fromFoldable)
import Data.Tuple (Tuple(..))
import Debug.Trace (traceAnyA)
import Network.HTTP.Affjax (AJAX)
import Pux (FoldP, noEffects, onlyEffects)
import Pux.DOM.Events (onClick)
import Pux.DOM.HTML (HTML, mapEvent)
import Signal.Channel (CHANNEL, send)
import Strophe (HTTP, Status, buildStanza, c, iq)
import Text.Smolder.HTML (button, h1, li, ul)
import Text.Smolder.Markup (text, (#!))

type State =
  { connection ∷ Connection.State
  , loginForm ∷ LoginForm.State
  , log ∷ List Status
  }

data Action
  = LoginFormAction LoginForm.Action
  | ConnectionAction Connection.Action

loginFormL :: forall a b r. Lens.Lens { "loginForm" :: a | r } { "loginForm" :: b | r } a b
loginFormL = Lens.lens _."loginForm" (_ { "loginForm" = _ })

_LoginFormAction :: Lens.Prism' Action LoginForm.Action
_LoginFormAction = Lens.prism LoginFormAction unwrap
  where
    unwrap (LoginFormAction x) = Either.Right x
    unwrap y = Either.Left y

connectionL :: forall a b r. Lens.Lens { "connection" :: a | r } { "connection" :: b | r } a b
connectionL = Lens.lens _."connection" (_ { "connection" = _ })

_ConnectionAction :: Lens.Prism' Action Connection.Action
_ConnectionAction = Lens.prism ConnectionAction unwrap
  where
    unwrap (ConnectionAction x) = Either.Right x
    unwrap y = Either.Left y

-- update ∷ ∀ eff. Action → State → EffModel State Action (http ∷ HTTP, ajax ∷ AJAX | eff)
update ∷ ∀ eff. FoldP State Action (http ∷ HTTP, ajax ∷ AJAX, console ∷ CONSOLE, channel ∷ CHANNEL, dom ∷ DOM, err ∷ EXCEPTION | eff)
update a@(LoginFormAction (LoginForm.Login jid password)) state =
  let
    connectionEffModel =
      Connection.update (Connection.Connect jid password) (Lens.view connectionL state)
  in
    focusEffModel connectionL _ConnectionAction connectionEffModel state
update a s =
  runUpdateFun (loginFormUpdateFun <> connectionUpdateFun <> updateLogUpdateFun <> sendPingOnConnectUpdateFun) a s
 where
  updateLog ∷ ∀ e. FoldP (List Status) Connection.Action e
  updateLog (Connection.StatusChange status) l = noEffects (status : l)
  updateLog _ l = noEffects l

  logL :: forall a b r. Lens.Lens { "log" :: a | r } { "log" :: b | r } a b
  logL = Lens.lens _."log" (_ { "log" = _ })

  sendPingOnConnect ∷ ∀ e. FoldP (Maybe Strophe.Connection) Connection.Action (http ∷ HTTP | e)
  sendPingOnConnect (Connection.StatusChange Strophe.Connected) conn@(Just connection) =
    onlyEffects conn [liftEff (traceAnyA "SENDING PING" >>= const ping >>= Strophe.send connection >>= const (pure Nothing))]
   where
    ping = runST (do
      b ← iq (fromFoldable [Tuple "to" "localhost", Tuple "type" "get", Tuple "id" "iq1"])
      c b "ping" (fromFoldable [Tuple "xmlns" "urn:xmpp:ping"])
      buildStanza b)
  sendPingOnConnect _ c = noEffects c

  updateLogUpdateFun = UpdateFun (focus logL _ConnectionAction updateLog)
  loginFormUpdateFun = UpdateFun (focus loginFormL _LoginFormAction LoginForm.update)
  connectionUpdateFun = UpdateFun (focus connectionL _ConnectionAction Connection.update)
  sendPingOnConnectUpdateFun = UpdateFun (focus (connectionL <<< connectionL) _ConnectionAction sendPingOnConnect)


view ∷ State → HTML Action
view { connection : { status }, loginForm, log } = do
  case status of
    Strophe.Connected →
      h1 $ do
        text "connected"
        button #! onClick (pure (ConnectionAction Connection.Disconnect)) $ text "disconnect"
    Strophe.Disconnecting →
      h1 $ text "disconnecting..."
    otherwise →
      mapEvent LoginFormAction (LoginForm.view loginForm (authStatus status))
  case log of
    Nil → pure unit
    otherwise →
      ul $ do
        for_ log $ \s →
          li $ text (show s)
 where
  authStatus Strophe.Attached = Just LoginSucceed
  authStatus Strophe.Authenticating = Just Authenticating
  authStatus Strophe.Authfail = Just (LoginFailed)
  authStatus Strophe.Connected = Just Authenticating
  authStatus Strophe.Connecting = Just (LoginError "connecting")
  authStatus Strophe.Connfail = Just (LoginError "Could not connect")
  authStatus Strophe.Conntimeout = Just (LoginError "Connection timeout")
  authStatus Strophe.Disconnected = Just (LoginError "Disconnected")
  authStatus Strophe.Disconnecting = Just (LoginError "Disconnecting")
  authStatus Strophe.Error = Just (LoginError "Connection problem")
