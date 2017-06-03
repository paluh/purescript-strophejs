module Strophe.Sugar where

send ∷ Connction → OutgoingStanza → Eff (http ∷ HTTP, connection :: CONNECTION | eff) Stanza
send conn (OutgoingIqRequest { payload, requestType}
