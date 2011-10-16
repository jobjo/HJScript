{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- Module      :  HJScript.Ajax
-- Copyright   :  (c) Joel Björnson 2006
-- License     :  BSD-style
-- Maintainer  :  Joel Björnson, joel.bjornson@gmail.com
-- Stability   :  experimental
-----------------------------------------------------------------------------

module HJScript.Ajax 
  (
    -- * Data
    ReqParam,ReqParams, IsReqParams(..),noParams,
    
    -- * High level
    asyncGetReq, asyncPostReq,

    -- * Parameters
    (=:), (<&>), addGetParams,
    
    -- * Low level,
    openAsync,openAsyncPost,sendNull,sendPost,
    setCallBack, succCallBack,isReady, isReadySucc,
    crtXMLHttpRequest, setPostReqHeader,
    
    module HJScript.Objects.XMLHttpRequest
  
  ) where

import HJScript.Lang

import HJScript.Objects.XMLHttpRequest
import HJScript.Objects.ActiveXObject
import HJScript.DOM

import Data.List (intersperse)

import Control.Monad.Trans

-- | Http
data HttpMethod 
  = Get 
  | Post 
  deriving Show


-- | Allowing path selections form XMLHttpRequest objects.
--instance HasDomSel (JsObject a XMLHttpRequest) where
--  toDomElement req = req # responseXML # documentElement 
  
  
----------------------------------------------------
-- High level interface
----------------------------------------------------

-- Get request
asyncGetReq :: (IsReqParams ps, IsExp e String) =>
              e  -> 
              ps -> 
              (JObject XMLHttpRequest -> HJScript ()) ->
              HJScript ()

asyncGetReq url params callb = do
  req <- crtXMLReq
  req # openAsyncGet url'
  req # setCallBack callb'
  req # sendNull
  where
    url' = url `addGetParams` params  
    callb' req = doIf (isReadySucc req) (callb req) noElse
    
  
-- | Post request
asyncPostReq ::  (IsReqParams ps, IsExp e String) =>
                e  -> 
                ps -> 
                (JObject XMLHttpRequest -> HJScript ()) ->
                HJScript ()

asyncPostReq url params callb = do
  req <- crtXMLReq
  req # openAsync Post url
  req # setCallBack callb'
  req # setPostReqHeader
  req # sendPost (toReqParams params)
  where
    callb' req = doIf (isReadySucc req) (callb req) noElse





----------------------------------------------------
-- Type ReqParam
----------------------------------------------------
type ReqParam  = (JString, JString)
type ReqParams = [ReqParam]

noParams :: ReqParams
noParams = []

class IsReqParams a where
  toReqParams :: a -> [ReqParam]
  
instance (IsExp e1 String, IsExp e2 String) => IsReqParams (e1,e2) where
  toReqParams (e1,e2) = [(toExp e1, toExp e2)]

instance IsReqParams ReqParams where
  toReqParams = id



-- Operator to add a paramname and a paramvalue
(=:) :: IsReqParams (a, b) => a -> b -> ReqParams
e1 =: e2 = toReqParams (e1,e2)


-- Operator to add params
(<&>) :: (IsReqParams p1 , IsReqParams p2) => p1 -> p2 -> ReqParams
p1 <&> p2 = (toReqParams p1) ++ (toReqParams p2)


instance IsExp ReqParam String  where
  toExp (p,v) = p .+. string "=" .+. v

instance IsExp [ReqParam] String where
  toExp pvs = foldr (.+.) (string "") pvs'
    where
      pvs' = intersperse (string "&") (map toExp pvs)

addGetParams :: (IsExp e String , IsReqParams ps) =>  e -> ps -> JString
addGetParams url params
  | null params' = toExp url
  | otherwise    = toExp url .+. string "?" .+. toExp params'
  where
    params' =  toReqParams params

  
----------------------------------------------------
-- Helpers..
----------------------------------------------------
openAsync :: (IsExp e String) =>
             HttpMethod -> e -> JObject XMLHttpRequest -> HJScript ()

openAsync meth url = openReq (toExp $ show meth) (toExp url) true

openAsyncGet :: (IsExp e String) =>
              e -> JObject XMLHttpRequest -> HJScript ()
openAsyncGet = openAsync Get

openAsyncPost :: (IsExp e String) =>
              e -> JObject XMLHttpRequest -> HJScript ()
openAsyncPost = openAsync Post

sendNull :: JObject XMLHttpRequest -> HJScript ()
sendNull = sendReq jnull



-- Sends post data
sendPost pst = sendReq $ toExp pst

-- SetCallBack
setCallBack fun req = do
  callback <- procedure $ \() -> fun req
  req # onReadyStateChange .=. callback
  where
    callback = procedure $ \() -> fun req


succCallBack :: JObject XMLHttpRequest -> JBool
succCallBack req = req # statusReq .==. int 200


isReady :: JObject XMLHttpRequest -> JBool
isReady req = req # readyState .==. int 4 


isReadySucc req = isReady req .&&. succCallBack req

-- | Creates a new XMLHttpRequest
crtXMLHttpRequest :: HJScript (Exp XMLHttpRequest)
crtXMLHttpRequest = new XMLHttpRequest ()


crtXMLReq :: HJScript (Exp XMLHttpRequest)
crtXMLReq = do
  req <- var
  doIf hasXMLHttpReq
      (new XMLHttpRequest () >>= \xmlHttp -> (req .=. xmlHttp)) $
    doElse $ 
      doIf hasActiveX 
        (new ActiveXObject msXMLHttp >>= \actX -> (req .=. (castObject actX)))  $
      doElse $ window # alert (string "JavaScript operation not supported")
  return (val req)

-- Sets post request
setPostReqHeader req = req # setRequestHeader contt appl
  where
    contt = string "Content-Type"
    appl  = string "application/x-www-form-urlencoded"

-- Is XMLHttpRequest implemented ?
hasXMLHttpReq, hasActiveX :: JBool
hasXMLHttpReq = window `hasFeature` XMLHttpRequest

hasActiveX = window `hasFeature` ActiveXObject
