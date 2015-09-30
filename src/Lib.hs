{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE TupleSections              #-}

module Lib (
  withLdap,
  Username(..),
  ProjectId(..),
  attrMatch,
  pluckAttr,
  getMemberUid,
  getSshPublicKey,
  listUsers,
  listUserData,

  Ldap, Host(..), PortNumber, Dn(..), LdapError, Attr(..), SearchEntry(..)
  ) where

import           Data.ByteString    (ByteString)
import           Data.Hashable      (Hashable (..))
import           Data.Monoid
import           Data.Text          (Text)
import qualified Data.Text          as T
import qualified Data.Text.Encoding as T
import qualified Data.Text.IO       as T
import           Ldap.Client


newtype Username = Username Text
                 deriving (Eq, Show, Hashable)
newtype ProjectId = ProjectId ByteString
                  deriving (Eq, Show, Hashable)

-- attrs :: AttrList NonEmpty
-- attrs  = undefined

attrMatch :: Attr -> (Attr, f AttrValue) -> Bool
attrMatch a0 (a1, _) = a0 == a1

pluckAttr :: Attr -> [(Attr, [AttrValue])] -> [AttrValue]
pluckAttr name attrlist =
  concat
  . map snd
  $ filter (attrMatch name) attrlist

getMemberUid :: SearchEntry -> [Username]
getMemberUid (SearchEntry _ attrlist) =
  map (Username . T.decodeUtf8)
  $ pluckAttr (Attr "memberUid") attrlist


getSshPublicKey :: SearchEntry -> [ByteString]
getSshPublicKey (SearchEntry _ vs) =
  pluckAttr (Attr "sshPublicKey") vs


listUsers :: Ldap -> Dn -> ProjectId -> IO [Username]
listUsers token  dn (ProjectId project) =
  (concat . map getMemberUid) <$> search token dn mempty filters []
  where
    filters = (Attr "cn") := project


listUserData :: Ldap
             -> Dn
             -> (SearchEntry -> [a])
             -> Username
             -> IO [a]
listUserData token dn getter u@(Username uid) = do
  let filters = (Attr "cn") := T.encodeUtf8 uid
  (concat . map getter) <$> search token dn mempty filters []


withLdap :: Host -> PortNumber -> (Ldap -> IO a) -> IO (Either LdapError a)
withLdap = with
