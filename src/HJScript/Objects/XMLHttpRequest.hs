{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Module      :  HJScript.Objects.XMLHttpRequest 
-- Copyright   :  (c) Joel Björnson 2006
-- License     :  BSD-style
-- Maintainer  :  Joel Björnson, joel.bjornson@gmail.com
-- Stability   :  experimental
-- XMLHttpRequest implementation
-----------------------------------------------------------------------------
module HJScript.Objects.XMLHttpRequest 
  (
    -- * Data
    XMLHttpRequest(..),
    
    -- * Properties
    statusReq,
    onReadyStateChange,
    responseXML,
    responseText,
    readyState,
        
    -- * Methods
    openReq,
    sendReq,
    setRequestHeader    
    
  ) where
  
import HJScript.Lang
import HJScript.DOM.Document (Document)

-- | XMLHttpRequest
data XMLHttpRequest = XMLHttpRequest deriving Show

instance IsClass XMLHttpRequest
instance HasConstructor XMLHttpRequest () ()

----------------------------------------------------
-- Special XMLHttpRequest properties
----------------------------------------------------

statusReq :: JObject XMLHttpRequest -> JInt
statusReq = deref "status"

onReadyStateChange :: JObject XMLHttpRequest -> Var (() -> ())
onReadyStateChange = derefVar "onreadystatechange"

responseXML :: JObject XMLHttpRequest -> JObject Document
responseXML = deref "responseXML"

responseText :: JObject XMLHttpRequest -> JString
responseText = deref "responseText"

readyState :: JObject XMLHttpRequest -> JInt
readyState = deref "readyState"

----------------------------------------------------
-- Special XMLHttpRequest methods
----------------------------------------------------

openReq :: JString -> JString -> JBool -> JObject XMLHttpRequest -> HJScript ()
openReq a1 a2 a3 = callVoidMethod "open" (a1, a2, a3)

sendReq :: JString ->  JObject XMLHttpRequest -> HJScript ()
sendReq  = callVoidMethod "send"

setRequestHeader :: JString -> JString -> JObject XMLHttpRequest -> HJScript ()
setRequestHeader = curry $ callVoidMethod "setRequestHeader"
