{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Objects.Boolean
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Objects.Boolean
  (
    Boolean(..),
    booleanToString, valueOf
  ) where

import HJScript.Lang

data Boolean = Boolean deriving Show
instance IsClass Boolean
instance IsDeref Boolean

-- | Constructors
instance HasConstructor Boolean JBool Bool
instance HasConstructor Boolean JString String
instance HasConstructor Boolean JInt Int

-- Methods
booleanToString :: JObject Boolean -> JString
booleanToString = methodCallNoArgs "toString"

valueOf :: JObject Boolean -> JBool
valueOf = methodCallNoArgs "valueOf"
