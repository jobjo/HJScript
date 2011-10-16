module HJScript.DOM.TextNode 
  (
        TextNode(..),
        
        text, length,
        
        appendData, deleteData, insertData,
        replaceData, splitText, substringData

  ) where

import HJScript.Lang
import HJScript.DOM.NodeTypes
import HJScript.DOM.Node

import Prelude hiding (length)

----------------------------------------------------
-- the TextNode object type
----------------------------------------------------
-- data TextNode = TextNode deriving Show (in NodeTypes)

instance IsClass TextNode

instance IsNode TextNode

----------------------------------------------------
-- properties
----------------------------------------------------

-- data is a keyword
text :: Exp TextNode -> Var String
text = derefVar "data"

length :: Exp TextNode -> JInt
length = deref "length"

----------------------------------------------------
-- methods
----------------------------------------------------

appendData :: JString -> Exp TextNode -> HJScript ()
appendData = callVoidMethod "appendData"

deleteData :: JInt -> JInt -> Exp TextNode -> HJScript ()
deleteData = curry $ callVoidMethod "deleteData"

insertData :: JInt -> JString -> Exp TextNode -> HJScript ()
insertData = curry $ callVoidMethod "insertData"

replaceData :: JInt -> JInt -> JString -> Exp TextNode -> HJScript ()
replaceData st ln txt = callVoidMethod "replaceData" (st,ln,txt) -- curry3 !!

splitText :: JInt -> Exp TextNode -> Exp TextNode
splitText = methodCall "splitText"

substringData :: JInt -> JInt -> Exp TextNode -> JString
substringData = curry $ methodCall "substringData"