{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Object.Date
-- Copyright   :  (c) Joel Bjornson 2008
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Objects.Date 
  (
    Date(..),
    
    -- * Get methods
    getDate,
    getDay, 
    getMonth,
    getFullYear,
    getHours,
    getMinutes,
    getSeconds,
    getMilliseconds,
    getUTCDay, 
    getUTCMonth,
    getUTCFullYear,
    getUTCHours, 
    getUTCMinutes, 
    getUTCSeconds,
    getUTCMilliseconds,
    getTime, 
    getTimezoneOffset, 
    getUTCDate, 

    -- * Set methods
    setDate,
    setMonth,
    setFullYear, 
    setYear, 
    setHours, 
    setMinutes, 
    setSeconds, 
    setMilliseconds, 
    setTime, 
    setUTCDate, 
    setUTCMonth,
    setUTCFullYear,
    setUTCHours,
    setUTCMinutes,
    setUTCSeconds,
    setUTCMilliseconds, 
    dateToString,
    toGMTString,
    toUTCString,
    toLocaleString
  
  ) where

import HJScript.Lang


data Date = Date deriving Show
instance IsClass Date


-- | Constructors for Date
instance HasConstructor Date () ()

-- * Get functions
getDate :: JObject Date -> JInt
getDate = methodCallNoArgs "getDate"

getDay :: JObject Date -> JInt
getDay = methodCallNoArgs "getDay"

getMonth :: JObject Date -> JInt
getMonth = methodCallNoArgs "getMonth"

getFullYear :: JObject Date -> JInt
getFullYear = methodCallNoArgs "getFullYear"

getHours :: JObject Date -> JInt
getHours = methodCallNoArgs "getHours"

getMinutes :: JObject Date -> JInt
getMinutes = methodCallNoArgs "getMinutes"

getSeconds ::JObject Date -> JInt
getSeconds = methodCallNoArgs "getSeconds"

getMilliseconds :: JObject Date -> JInt
getMilliseconds = methodCallNoArgs "getMilliseconds"

getUTCDay :: JObject Date -> JString 
getUTCDay = methodCallNoArgs "getUTCDay"

getUTCMonth :: JObject Date -> JInt
getUTCMonth = methodCallNoArgs "getUTCMonth"

getUTCFullYear :: JObject Date -> JInt
getUTCFullYear = methodCallNoArgs "getUTCFullYear"

getUTCHours :: JObject Date -> JString 
getUTCHours = methodCallNoArgs "getUTCHours"

getUTCMinutes :: JObject Date -> JString 
getUTCMinutes = methodCallNoArgs "getUTCMinutes"

getUTCSeconds :: JObject Date -> JString 
getUTCSeconds = methodCallNoArgs "getUTCSeconds"

getUTCMilliseconds :: JObject Date -> JString 
getUTCMilliseconds = methodCallNoArgs "getUTCMilliseconds"

getTime :: JObject Date -> JString 
getTime = methodCallNoArgs "getTime"

getTimezoneOffset :: JObject Date -> JString 
getTimezoneOffset = methodCallNoArgs "getTimezoneOffset"

getUTCDate :: JObject Date -> JString 
getUTCDate = methodCallNoArgs "getUTCDate"


-- * Set functions
setDate :: JInt -> JObject Date -> HJScript ()
setDate = callVoidMethod "setDate"

setMonth :: JInt -> JObject Date -> HJScript ()
setMonth = callVoidMethod "setMonth"

setFullYear ::  JInt -> JObject Date -> HJScript ()
setFullYear = callVoidMethod "setFullYear"

setYear ::  JInt -> JObject Date -> HJScript ()
setYear = callVoidMethod "setYear"

setHours ::  JInt -> JObject Date -> HJScript ()
setHours = callVoidMethod "setHours"

setMinutes ::  JInt -> JObject Date -> HJScript ()
setMinutes = callVoidMethod "setMinutes"

setSeconds ::  JInt -> JObject Date -> HJScript ()
setSeconds = callVoidMethod "setSeconds"

setMilliseconds ::  JInt -> JObject Date -> HJScript ()
setMilliseconds = callVoidMethod "setMilliseconds"

setTime ::  JInt -> JObject Date -> HJScript ()
setTime = callVoidMethod "setTime"

setUTCDate ::  JInt -> JObject Date -> HJScript ()
setUTCDate = callVoidMethod "setUTCDate"

setUTCMonth ::  JInt -> JObject Date -> HJScript ()
setUTCMonth = callVoidMethod "setUTCMonth"

setUTCFullYear ::  JInt -> JObject Date -> HJScript ()
setUTCFullYear = callVoidMethod "setUTCFullYear"

setUTCHours ::  JInt -> JObject Date -> HJScript ()
setUTCHours = callVoidMethod "setUTCHours"

setUTCMinutes ::  JInt -> JObject Date -> HJScript ()
setUTCMinutes = callVoidMethod "setUTCMinutes"

setUTCSeconds ::  JInt -> JObject Date -> HJScript ()
setUTCSeconds = callVoidMethod "setUTCSeconds"

setUTCMilliseconds ::  JInt -> JObject Date -> HJScript ()
setUTCMilliseconds = callVoidMethod "setUTCMilliseconds"

dateToString :: JObject Date -> JString 
dateToString = methodCallNoArgs "toString"

toGMTString :: JObject Date -> JString 
toGMTString = methodCallNoArgs "toGMTString"

toUTCString :: JObject Date -> JString 
toUTCString = methodCallNoArgs "toUTCString"

toLocaleString :: JObject Date -> JString 
toLocaleString = methodCallNoArgs "toLocaleString"
