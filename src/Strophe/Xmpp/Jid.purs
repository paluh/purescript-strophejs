module Strophe.Xmpp.Jid where

import Prelude
import Data.Generic.Rep (class Generic)
import Data.Maybe (Maybe(..))
import Data.Newtype (class Newtype, unwrap, wrap)

newtype NonEmptyString = NonEmptyString String
derive instance newtypeNonEmptyString ∷ Newtype NonEmptyString _
derive instance eqNonEmptyString ∷ Eq NonEmptyString
derive instance ordNonEmptyString ∷ Ord NonEmptyString
derive newtype instance semigroupNonEmptyString ∷ Semigroup NonEmptyString

nonEmptyString ∷ String → Maybe NonEmptyString
nonEmptyString "" = Nothing
nonEmptyString v = Just <<< wrap $ v

fromNonEmptyString ∷ NonEmptyString → String
fromNonEmptyString = unwrap

type Jid = String

-- newtype Jid =
--   Jid
--     { localpart ∷ Maybe NonEmptyString
--     , domainpart ∷ NonEmptyString
--     , resourcepart ∷ Maybe NonEmptyString
--     }
-- derive instance genericJid ∷ Generic Jid _
-- derive instance newtypeJid ∷ Newtype Jid _
-- derive instance eqJid ∷ Eq Jid
-- derive instance ordJid ∷ Ord Jid


-- | Parse the parts of a JID. The parts need to be validated with stringprep
-- before the JID can be constructed
-- jidParts :: AP.Parser (Maybe Text, Text, Maybe Text)
-- jidParts = do
--     maybeLocalPart <- Just <$> localPart <|> return Nothing
--     domainPart <- AP.takeWhile1 (AP.notInClass ['@', '/'])
--     maybeResourcePart <- Just <$> resourcePart <|> return Nothing
--     AP.endOfInput
--     return (maybeLocalPart, domainPart, maybeResourcePart)
--   where
--     localPart = do
--         bytes <- AP.takeWhile1 (AP.notInClass ['@', '/'])
--         _ <- AP.char '@'
--         return bytes
--     resourcePart = do
--         _ <- AP.char '/'
--         AP.takeText
