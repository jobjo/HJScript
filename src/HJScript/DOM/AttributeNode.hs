module HJScript.DOM.AttributeNode 
    (
        AttributeNode(..),
        
        name, specified, value        

    ) where

import HJScript.Lang

import HJScript.DOM.NodeTypes
import HJScript.DOM.Node


----------------------------------------------------
-- the AttributeNode object type
----------------------------------------------------
-- data AttributeNode = AttributeNode deriving Show (in NodeTypes

instance IsClass AttributeNode

-- inherit all the Node properties and methods
instance IsNode AttributeNode

----------------------------------------------------
-- properties
----------------------------------------------------

name :: Exp AttributeNode -> JString
name = deref "name"

specified :: Exp AttributeNode -> JBool
specified = deref "specified"

value :: Exp AttributeNode -> Var String
value = derefVar "value"
