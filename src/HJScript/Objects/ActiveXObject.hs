{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Module      :  HJScript.Objects.ActiveXObject
-- Copyright   :  (c) Joel Björnson 2006
-- License     :  BSD-style
-- Maintainer  :  Joel Björnson, joel.bjornson@gmail.com
-- Stability   :  experimental
-- Microsoft active-x object.
-----------------------------------------------------------------------------
module HJScript.Objects.ActiveXObject 
  (
    ActiveXObject(..),
    msXMLHttp
  ) where


import HJScript.Lang


-- ActiveXObject
data ActiveXObject = ActiveXObject deriving Show
instance IsClass ActiveXObject
instance HasConstructor ActiveXObject JString String


msXMLHttp = string "Microsoft.XMLHTTP"
