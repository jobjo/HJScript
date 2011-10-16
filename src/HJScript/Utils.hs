-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Utils
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Utils
  (

    -- Elements and properties
    dispNone, dispBlock,dispInline, 
    thisElem, thisXHTMLElement, thisNode, 
    
    -- Predicates
    hasClass, hasChild, isVisible, isInVisible, 
    
    -- Pures methods for accessing elements and values.
    elemById, elemsByTag, fstElemByTag, allElems, parentElem, elemVal,
    
    -- HJScript monadic methods for accessing elements and changing properties.
    msg, getElemById, getElemsByTag, getFstElemByTag, getAllElems, getParentElem, 
    getElemsByClass, getFstElemByClass, getSiblings, hideElem , showElem, 
    showInline, showBlock , remFirstChild, remChildren, remElem, 
    appendChildren, setChild, toggleVis, toggleVisBlock, toggleVisInline, setVal 
  ) where

import HJScript.Lang
import HJScript.Objects.Array
import HJScript.DOM

-- Display properties
dispNone :: JString
dispNone    = string "none"

dispBlock :: JString
dispBlock   = string "block"

dispInline :: JString
dispInline  = string "inline"

thisElem :: Exp ElementNode
thisElem = JThis

thisXHTMLElement :: Exp ElementNode
thisXHTMLElement = JThis

thisNode :: Exp Node
thisNode = JThis

--------------------------------------------------------------------------------
-- Properties
--------------------------------------------------------------------------------

hasClass :: (IsJString e) => e -> Exp ElementNode-> JBool
hasClass name elem = (val $ elem' # className) .==. (toJString name) 
  where elem' = asXHTMLElement elem

hasChild :: IsElementNode e => Exp e -> JBool
hasChild elem = elem # firstChild .!=. jnull

isVisible :: IsElementNode n => Exp n -> JBool
isVisible elem = (val $ elem # style # display) .!=. dispNone

isInVisible :: IsElementNode n => Exp n -> JBool
isInVisible elem = (val $ elem # style # display) .==. dispNone


--------------------------------------------------------------------------------
-- Pure functions
--------------------------------------------------------------------------------
elemById :: IsJString e => e -> Exp ElementNode
elemById id = document # getElementById (toJString id)

elemsByTag :: IsJString e => e -> JArray ElementNode
elemsByTag name = document # getElementsByTagName (toJString name)

fstElemByTag :: IsJString e => e -> Exp ElementNode
fstElemByTag = val . headArr . elemsByTag

allElems :: JArray ElementNode
allElems = elemsByTag (string "*")  

parentElem :: IsElementNode e => (Exp e) -> Exp ElementNode
parentElem elem = castObject $ elem # parentNode

-- Creating elements

txtNode :: IsJString s => s -> Exp TextNode
txtNode str = document # createTextNode (toJString str)

elemNode :: IsJString s => s -> Exp ElementNode
elemNode str = document # createElement (toJString str)

--------------------------------------------------------------------------------
-- Non pure funtions
--------------------------------------------------------------------------------

-- Alerts a message
msg :: IsExp e t => e -> HJScript ()
msg e = window # alert (toExp e)

getElemById :: IsJString e => e -> HJScript (Exp ElementNode)
getElemById = return . elemById

getElemsByTag :: IsJString e => e -> HJScript (JArray ElementNode)
getElemsByTag = return . elemsByTag

getFstElemByTag :: IsJString e => e -> HJScript (Exp ElementNode)
getFstElemByTag = return . fstElemByTag

getAllElems :: HJScript (JArray ElementNode)
getAllElems = return allElems

getParentElem :: IsElementNode e => Exp e -> HJScript (Exp ElementNode)
getParentElem  = return . parentElem

getElemsByClass :: (IsJString e) => e -> HJScript (JArray ElementNode)
getElemsByClass c = filterArray (hasClass c) allElems

getFstElemByClass :: IsJString e => e -> HJScript (JObject ElementNode)
getFstElemByClass c = do
  elems <- getElemsByClass c
  return $ val $ headArr elems

-- Get all siblings to an element.
getSiblings :: IsElementNode e => Exp e -> HJScript (JArray Node)
getSiblings elem = do
  elems <- varWith $ elem # parentElem # childNodes
  filterArray (.!=. thisNode) (val elems)

-- Hide an element by setting its's style display 
-- property to "none"
hideElem :: IsElementNode e => Exp e -> HJScript ()
hideElem elem = elem # style # display .=. dispNone

-- Shows an element by setting it's style disply property to "block".
showElem :: IsElementNode e => Exp e -> HJScript ()
showElem = showBlock

-- Shows an element by setting it's styel display property to "inline".
showInline :: IsElementNode e => Exp e -> HJScript ()
showInline elem = elem # style # display .=. dispInline

-- Shows an element by setting it's styel display property to "block".
showBlock :: IsElementNode e => Exp e -> HJScript ()
showBlock elem = elem # style # display .=. dispBlock

-- Removes the first children of an element
remFirstChild :: IsElementNode e => Exp e -> HJScript ()
remFirstChild elem = elem # removeChild (elem # firstChild)
     
-- Removes all children
remChildren :: IsElementNode e => Exp e -> HJScript ()
remChildren elem = 
  while (elem # hasChild) $ 
    remFirstChild elem

-- Remove an element
remElem :: IsElementNode e => Exp e -> HJScript ()
remElem elem = elem # parentElem # removeChild elem'
  where elem' = castObject elem :: Exp Node

appendChildren :: (IsElementNode n, IsNode t) => JArray t -> Exp n -> HJScript ()
appendChildren childs elem = do
  mapArrayH_ (\child -> elem # appendChild child) childs

-- Remove all children and add a new child element.
setChild :: (IsNode n1, IsElementNode n2) => Exp n1 -> Exp n2 -> HJScript ()
setChild e1 e2 = do
  e2 # remChildren
  e2 # appendChild e1
  
-- Toggle visible function
toggleVis :: (IsElementNode n) => Exp n -> HJScript ()
toggleVis = toggleVisBlock

toggleVisBlock :: (IsElementNode n) => Exp n -> HJScript ()
toggleVisBlock elem =  
  (isInVisible elem) ? (elem # showBlock ) <|> (elem # hideElem)


toggleVisInline :: (IsElementNode n) => Exp n -> HJScript ()
toggleVisInline elem =  
  (isInVisible elem) ? (elem # showInline ) <|> (elem # hideElem)

elemVal :: IsElementNode n => Exp n -> JString
elemVal elem = elem # (deref "value")

setVal :: IsElementNode n => JString -> Exp n -> HJScript ()
setVal val elem = elem # (derefVar "value") .=. val

newTxtNode :: IsJString s => s -> HJScript (Exp TextNode)
newTxtNode = inVar . txtNode

newElem :: IsJString s => s -> HJScript (Exp ElementNode)
newElem = inVar . elemNode


--------------------------------------------------------------------------------
-- Helpers
--------------------------------------------------------------------------------

-- Should be placed in a another module, but the problem is where.
-- Not in ElementNode since it requries Style.
style :: IsElementNode e => Exp e -> Exp Style
style = deref "style"

display :: Exp Style -> Var String
display = derefVar "display"
