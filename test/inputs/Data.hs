{-# LANGUAGE DeriveDataTypeable         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE ForeignFunctionInterface   #-}
{-# LANGUAGE GADTs                      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeSynonymInstances       #-}
{-# LANGUAGE ScopedTypeVariables        #-}

module Data where

-----------------------------------------------------------------------------
import           Control.Concurrent
import Control.Exception (throw)
import qualified Control.Monad.Catch as E
import           Control.Monad                (forM, when)
import           Control.Monad.Trans.Reader   (ReaderT (..))
import           Data.Aeson                   (FromJSON (parseJSON),
                                               ToJSON (toJSON), Value (String, Object, Array, Number),
                                               object, (.:), (.:?), (.=))
import           Data.Scientific              (floatingOrInteger)
import           Data.Hashable
import qualified Data.HashTable.IO            as H
import           Data.IntervalMap.Strict      (IntervalMap)
import           Data.Text                    (Text)
import qualified Data.Vector.Storable.Mutable as VM
import           Data.Word                    (Word8)
import qualified Data.Vector         as V
-----------------------------------------------------------------------------

