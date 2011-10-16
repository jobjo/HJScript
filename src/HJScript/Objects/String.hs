{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Object.String
-- Copyright   :  (c) Joel Bjornson 008
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module  HJScript.Objects.String
  (
    -- * String properties
    strLength,
    
    -- * String methods
    toUpperCase, toLowerCase, anchor, big,
    blink, bold, charAt, charCodeAt,
    strConcat, indexOf, italics,lastIndexOf,
    link, replace, substr, substring
  
  ) where

import HJScript.Lang

instance IsClass String


----------------------------------------------------
-- String properties
----------------------------------------------------

strLength :: JString -> JInt 
strLength = deref "length"


----------------------------------------------------
-- String methods
----------------------------------------------------
toUpperCase :: JString -> JString 
toUpperCase = callMethod "toUpperCase" ()


toLowerCase :: JString -> JString 
toLowerCase = callMethod "toLowerCase" ()

anchor :: JString -> JString 
anchor = callMethod "anchor" ()

big :: JString -> JString 
big = callMethod "big" ()

blink :: JString -> JString 
blink = callMethod "blink" ()

bold :: JString -> JString 
bold = callMethod "bold" ()

charAt :: JInt -> JString -> JString 
charAt = callMethod "charAt"

charCodeAt :: JInt -> JString -> JInt 
charCodeAt = callMethod "charCodeAt"

strConcat :: JString  -> JString  -> JString 
strConcat = callMethod "concat"

indexOf :: JString -> JInt 
indexOf = callMethod "indexOf" ()

italics :: JString -> JString 
italics = callMethod "italics" ()

lastIndexOf :: JString -> JInt 
lastIndexOf = callMethod "lastIndexOf" ()

link :: JString -> JString 
link = callMethod "link" ()

replace :: JString  -> JString -> JString 
replace = callMethod "replace"

substr :: JInt -> JString  -> JString 
substr = callMethod "substr"

substring :: JInt ->  JInt -> JString -> JString 
substring x y = callMethod "substring" (x,y)
