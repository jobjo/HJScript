-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.DOM.ElementNode
-- Copyright   :  (c) Joel Bjornson 2008
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.DOM.ElementNode 
  (
    ElementNode(..), IsElementNode,
    
    -- Properties
    attributes, childNodes,
    firstChild, lastChild, localName, nextSibling,
    parentNode, previousSibling, tagName, scrollTop, scrollTopVar, scrollHeight, scrollHeightVar,
  
    -- Methods
    appendChild, cloneNode, getAttribute, 
    getAttributeNode, hasAttribute, hasAttributes, 
    hasChildNodes, insertBefore, normalize,
    removeAttribute, removeAttributeNode, removeChild, replaceChild,
    setAttribute, setAttributeNode
  
  ) where

import HJScript.Lang
import HJScript.DOM.NodeTypes
import HJScript.DOM.Node
import HJScript.DOM.NamedNodeMap

----------------------------------------------------
-- The ElementNode type
----------------------------------------------------
-- data ElementNode = ElementNode deriving Show

instance IsClass ElementNode
instance IsNode ElementNode

class IsClass n => IsElementNode n
instance IsElementNode ElementNode

----------------------------------------------------
-- Properties
----------------------------------------------------
attributes :: IsElementNode n => Exp n -> Exp NamedNodeMap
attributes = deref "attributes"

childNodes :: IsElementNode n =>  Exp n -> JArray Node
childNodes = deref "childNodes"

firstChild :: IsElementNode n => Exp n -> Exp Node
firstChild = deref "firstChild"

lastChild ::  IsElementNode n => Exp n -> Exp Node
lastChild = deref "lastChild"

localName ::  IsElementNode n => Exp n -> JString
localName = deref "localName"

nextSibling :: IsElementNode n => Exp n -> Exp Node
nextSibling = deref "nextSibling"

parentNode :: IsElementNode n => Exp n -> Exp Node
parentNode = deref "parentNode"

previousSibling ::  IsElementNode n => Exp n -> Exp Node
previousSibling = deref "previousSibling"

tagName ::  IsElementNode o => Exp o -> JString
tagName = deref "tagName"

scrollTop :: IsElementNode o => Exp o -> Exp JInt
scrollTop = deref "scrollTop"

scrollTopVar :: IsElementNode o => Exp o -> Var JInt
scrollTopVar exp = JDerefVar exp "scrollTop"

scrollHeight :: IsElementNode o => Exp o -> Exp JInt
scrollHeight = deref "scrollHeight"

scrollHeightVar :: IsElementNode o => Exp o -> Var JInt
scrollHeightVar exp = JDerefVar exp "scrollHeight"



----------------------------------------------------
-- Methods
----------------------------------------------------
appendChild :: (IsElementNode n, IsNode c) => Exp c -> Exp n -> HJScript ()
appendChild = callVoidMethod "appendChild"

getAttribute :: IsElementNode n => JString -> Exp n -> JString 
getAttribute = methodCall "getAttribute"

getAttributeNode :: IsElementNode n => JString -> Exp n -> Exp AttributeNode
getAttributeNode = methodCall "getAttributeNode"

hasAttribute :: IsElementNode n => JString -> Exp n -> JBool 
hasAttribute = methodCall "hasAttribute"

hasAttributes :: IsElementNode n => Exp n -> JBool
hasAttributes = methodCall "hasAttributes" ()

hasChildNodes :: IsElementNode n => Exp n -> JBool 
hasChildNodes = methodCall "hasChildNodes" ()

insertBefore :: IsElementNode n => 
                Exp Node -> Exp Node -> Exp n -> HJScript ()
insertBefore b a = callVoidMethod "insertBefore" (b,a)

normalize :: IsElementNode n => Exp n -> HJScript ()
normalize = callVoidMethod "normalize" ()

removeAttribute :: IsElementNode n => JString -> Exp n -> HJScript ()
removeAttribute = callVoidMethod "removeAttribute"

removeAttributeNode :: IsElementNode n => Exp AttributeNode -> Exp n -> HJScript ()
removeAttributeNode = callVoidMethod "removeAttributeNode"

removeChild :: (IsElementNode n, IsNode c) => Exp c -> Exp n -> HJScript ()
removeChild = callVoidMethod "removeChild"

replaceChild :: (IsElementNode n, IsNode c, IsNode d) => 
                Exp c -> Exp d -> Exp n -> HJScript ()
replaceChild = curry $ callVoidMethod "replaceChild"

setAttribute :: IsElementNode n => JString -> JString -> Exp n -> HJScript ()
setAttribute = curry $ callVoidMethod "setAttribute"

setAttributeNode :: IsElementNode n => Exp AttributeNode -> Exp n -> HJScript ()
setAttributeNode = callVoidMethod "setAttributeNode"
