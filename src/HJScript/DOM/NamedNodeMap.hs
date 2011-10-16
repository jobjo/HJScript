module HJScript.DOM.NamedNodeMap where

import HJScript.Lang

import HJScript.DOM.NodeTypes (AttributeNode)

----------------------------------------------------
-- the NamedNodeMap object type
----------------------------------------------------
data NamedNodeMap = NamedNodeMap deriving Show

instance IsClass NamedNodeMap

----------------------------------------------------
-- properties
----------------------------------------------------

length :: Exp NamedNodeMap -> JInt
length = deref "length"

----------------------------------------------------
-- methods
----------------------------------------------------

getNamedItem :: JString -> Exp NamedNodeMap -> Exp AttributeNode
getNamedItem = methodCall "getNamedItem"

item :: JInt -> Exp NamedNodeMap -> Exp AttributeNode
item = methodCall "item"

removeNamedItem :: JString -> Exp NamedNodeMap -> Exp AttributeNode
removeNamedItem = methodCall "removeNamedItem"