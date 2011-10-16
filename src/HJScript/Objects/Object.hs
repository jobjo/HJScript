{-# LANGUAGE MultiParamTypeClasses #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Language.HJavaScript.Objects.Array
-- Copyright   :  (c) 2010 Joel Bjornson 
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Objects.Object where

import Language.HJavaScript.Syntax

-- | plain old Object
data Object = Object deriving Show
instance IsClass Object
instance HasConstructor Object () ()
