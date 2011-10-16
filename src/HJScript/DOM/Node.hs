module HJScript.DOM.Node (
        Node(..), IsNode(..),
        
        NodeType(..), nodeTypeVal,
        
        nodeName, nodeType, nodeValue, 
        ownerDocument, prefix, cloneNode
        ) where

import HJScript.Lang

--import HJScript.DOM.Document
import HJScript.DOM.NodeTypes

----------------------------------------------------
-- the Node object type
----------------------------------------------------
-- data Node = Node deriving Show (is in NodeTypes)

instance IsClass Node

class IsClass n => IsNode n where
 castToNode :: JObject n -> JObject Node
 castToNode = castObject
 castFromNode :: JObject Node -> JObject n
 castFromNode = castObject

instance IsNode Node where
 castToNode   = id
 castFromNode = id



----------------------------------------------------
-- Properties for Nodes
----------------------------------------------------

-- We move all properties dealing with children, siblings
-- and parents to ElementNode, since in our simple model 
-- they only make sense on (subclasses of) Element nodes 
-- anyway.

nodeName :: IsNode n => Exp n -> JString 
nodeName = deref "nodeName"

nodeType :: IsNode n => Exp n -> JInt 
nodeType = deref "nodeType"

nodeValue :: IsNode n => Exp n -> Var String 
nodeValue = derefVar "nodeValue"

ownerDocument ::  IsNode n => Exp n -> Exp Document
ownerDocument = deref "ownerDocument"

prefix :: IsNode n => Exp n -> Exp String
prefix = deref "prefix"

----------------------------------------------------
-- Methods for Nodes
----------------------------------------------------

cloneNode ::  IsNode n => JBool -> Exp n -> Exp n
cloneNode = methodCall "cloneNode"

