{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Data.Model where

import Database.Persist
import Database.Persist.TH
import Database.Persist.Sqlite
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Except
import Control.Monad.Reader
import Data.Typeable
import Data.Data
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Word
import Data.Derived
import Data.Time.Clock


share [mkPersist ( sqlSettings  ), mkMigrate "migrateAll"] [persistUpperCase|

User
   username T.Text
   email T.Text
   age Word8
   password B.ByteString
   salt B.ByteString
   joined UTCTime
   lastlogin UTCTime
   Primary email
 deriving Show Read Eq Ord Typeable Data

Post
    postNumber Int
    title T.Text
    spoiler T.Text
    content T.Text
    replyTo Int
    prevVersionId Int
    created UTCTime 
    threadid Int
    owneremail T.Text
    Primary postNumber
    Foreign User owner owneremail
    Foreign Thread thread threadid
    Foreign Post prevVersion prevVersionId
    Foreign Post inReplyTo replyTo
    Foreign Post linkedPosts postNumber 
 deriving Show Read Eq Ord Data Typeable

PostLinks
    linkedTo Int
    linkedFrom Int
    linkType LinkType
    Foreign Post toPost linkedTo
    Foreign Post fromPost linkedFrom
    Primary linkedFrom
 deriving Show Read Eq Ord Data Typeable

Thread
  threadNumber Int
  title T.Text
  description T.Text
  owneremail T.Text
  superThreadId Int 
  created UTCTime default=CURRENT_TIME
  updated UTCTime
  Primary threadNumber
  UniqueOwner owneremail threadNumber 
  Foreign Thread superThread superThreadId
  Foreign User owner owneremail
 deriving Show Read Eq Ord Data Typeable

Votes
   amount Word8
   postNumber Int
   owneremail T.Text
   cast UTCTime
   Foreign User owner owneremail
   Foreign Post post postNumber
   Primary postNumber owneremail
  deriving Show Read Eq Ord Data Typeable
|]





