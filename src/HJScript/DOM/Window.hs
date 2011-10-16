-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.DOM.Window
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.DOM.Window
  (
    -- Constructor function
    window, Window,
    
    -- Properties
    frames, closed, defaultStatus, winDocument, winLength,
    winName, opener, parent, self, status, top, blur, focus,
     
    -- Methods
    alert, close, confirm, createPopup, resizeBy, resizeTo,
    promt, moveBy, moveTo, navigate, open, scrollBy, scrollTo, 
  
  ) where
  
import HJScript.Lang
import HJScript.DOM.NodeTypes
import HJScript.DOM.XHTML

data Window = Window deriving Show
instance IsClass Window

-- Accessing the window object
window :: Exp Window
window = JConst "window"


-- Window properties
closed :: Exp Window -> JBool 
closed = deref "closed"

defaultStatus :: Exp Window -> JString 
defaultStatus = deref "defaultStatus"

winDocument :: Exp Window -> Exp Document
winDocument = deref "document"

frames :: Exp Window -> JArray Frame
frames = deref "frames"

winLength :: Exp Window -> Var Int 
winLength = derefVar "length"

winName :: Exp Window -> Var String 
winName = derefVar "name"

opener :: Exp Window -> Exp Window
opener = deref "opener"

parent :: Exp Window -> Exp Window
parent = deref "parent"

self :: Exp Window -> Exp Window
self = deref "self"

status :: Exp Window -> Var String 
status = derefVar "status"

top :: Exp Window -> Exp Window
top = deref "top"

-- Window methods
alert :: Exp t -> Exp Window -> HJScript ()
alert  = callVoidMethod "alert" 

blur :: Exp Window -> HJScript ()
blur = callVoidMethod "blur" ()

{- Not yet supported.
clearInterval, clearTimeout :: Exp Window -> HJScript ()
clearInterval = callVoidMethod "clearInterval"
clearTimeout  = callVoidMethod "clearTimeout"
-}

close :: Exp Window -> HJScript ()
close = callVoidMethod "close" ()

confirm :: JString -> Exp Window -> JBool 
confirm = methodCall "confirm"

createPopup :: Exp Window ->  Exp Window
createPopup = methodCall "createPopup" ()

focus ::  Exp Window -> HJScript ()
focus = callVoidMethod "focus" ()

moveBy :: JInt ->  JInt -> Exp Window -> HJScript ()
moveBy x y = callVoidMethod "moveBy" (x,y) 

moveTo :: JInt ->  JInt -> Exp Window -> HJScript ()
moveTo x y = callVoidMethod "moveTo" (x,y)

open :: JString -> JString -> Exp Window -> HJScript ()
open url sett  = callVoidMethod "open" (url, sett)

promt :: JString -> Exp Window -> JString  
promt = methodCall "promt"

-- Not standard?
navigate :: JString -> Exp Window -> HJScript ()
navigate = callVoidMethod "navigate"

scrollBy :: JInt ->  JInt -> Exp Window -> HJScript ()
scrollBy x y = callVoidMethod "scrollBy" (x,y)

scrollTo :: JInt ->  JInt -> Exp Window -> HJScript ()
scrollTo x y = callVoidMethod "scrollTo" (x,y)

resizeBy :: JInt ->  JInt -> Exp Window -> HJScript ()
resizeBy x y = callVoidMethod "resizeBy" (x,y)

resizeTo :: JInt ->  JInt -> Exp Window -> HJScript ()
resizeTo x y = callVoidMethod "resizeTo" (x,y)

{- Not yet supported
setInterval, setTimeout :: JString -> JInt -> Exp Window -> HJScript ()
setInterval = callVoidMethod "setInterval"
setTimeout  = callVoidMethod "setTimeout"
-}