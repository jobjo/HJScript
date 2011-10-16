-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.DOM.Document
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.DOM.Document 
  (
    -- Data
    Document,
    
    -- Constructor function
    document,
    
    -- Properties
    anchors, applets, embeds, forms, images, links, stylesheets,
    alinkColor, body, cookie, documentElement, domain, lastModified,
    linkColor, referrer, url, vlinkColor,
    
    -- Methods
    createAttribute, createElement, createTextNode, 
    getElementById, getElementsByTagName,
    write, writeln
    
  ) where
  
import HJScript.Lang
import HJScript.DOM.NodeTypes
import HJScript.DOM.XHTML

-- In NodeTypes to avoid cyclic dependency:
-- data Document = Document deriving Show
instance IsClass Document

-- Access the document object
document :: Exp Document
document = JConst "document"

----------------------------------------------------
-- Properties
----------------------------------------------------
anchors :: Exp Document ->  JArray Anchor
anchors = deref "anchors"

-- Not standard?
applets :: Exp Document -> JArray Object
applets = deref "applets"

-- Not standard?
embeds :: Exp Document -> JArray ElementNode
embeds = deref "embeds"

forms :: Exp Document -> JArray Form
forms = deref "forms"

images :: Exp Document -> JArray Image
images = deref "images"

links :: Exp Document -> JArray Link
links = deref "links"

-- Not standard?
stylesheets :: Exp Document -> JArray Style
stylesheets = deref "stylesheets"

-- Not standard?
alinkColor :: Exp Document -> JString 
alinkColor = deref "alinkColor"

body :: Exp Document -> Exp Body
body = deref "body"

cookie :: Exp Document -> Var String 
cookie = derefVar "cookie"

documentElement :: Exp Document -> Exp ElementNode
documentElement = deref "documentElement"

domain :: Exp Document -> JString 
domain = deref "domain"

lastModified :: Exp Document -> JString 
lastModified = deref "lastModified"

-- Not standard?
linkColor :: Exp Document -> JString 
linkColor = deref "linkColor"

referrer :: Exp Document -> JString 
referrer = deref "referrer"

url :: Exp Document -> JString 
url = deref "url"

title :: Exp Document -> JString 
title = deref "title"

vlinkColor :: Exp Document -> JString 
vlinkColor = deref "vlinkColor"

----------------------------------------------------
-- Methods
----------------------------------------------------
createAttribute :: JString -> Exp Document -> Exp AttributeNode
createAttribute = methodCall "createAttribute"

createElement :: JString -> Exp Document -> Exp ElementNode
createElement = methodCall "createElement"

createTextNode :: JString -> Exp Document -> Exp TextNode
createTextNode = methodCall "createTextNode"

getElementById :: JString -> Exp Document -> Exp ElementNode
getElementById = methodCall "getElementById"

getElementsByTagName :: JString -> Exp Document -> JArray ElementNode
getElementsByTagName = methodCall "getElementsByTagName"

write :: JString -> Exp Document -> HJScript ()
write = callVoidMethod "write"

writeln :: JString -> Exp Document -> HJScript ()
writeln = callVoidMethod "writeln"