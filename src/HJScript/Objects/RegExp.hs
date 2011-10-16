{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Objects.RegExp
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module  HJScript.Objects.RegExp
  (
    -- Data
    RegExp(..),

    -- Functions
    test, exec, compile
  ) where

import HJScript.Lang

data RegExp = RegExp deriving Show
instance IsClass RegExp
instance IsDeref RegExp

-- | Constructors for RegExp
instance HasConstructor RegExp JString String

-- Methods
test :: JString -> JObject RegExp -> JBool
test = callMethod "test"

exec :: JString -> JObject RegExp -> JString
exec = callMethod "test"

compile :: JString -> JObject RegExp -> HJScript ()
compile = callVoidMethod "compile"
