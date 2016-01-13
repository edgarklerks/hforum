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
module Data.Derived where

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
import Data.Time.Clock

data LinkType = Related | Reply 
        deriving (Show, Read, Eq, Ord, Data, Typeable, Enum)

derivePersistField "LinkType"



