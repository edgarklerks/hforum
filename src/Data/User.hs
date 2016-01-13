{-# LANGUAGE GADTs #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}
module Data.User where

import Database.Persist
import Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString as B
import qualified Data.Text as T
import Data.Word
import Control.Lens
import Data.Model
import Data.Time.Clock
import Control.Monad.Reader


