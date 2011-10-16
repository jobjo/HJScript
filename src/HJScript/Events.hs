-----------------------------------------------------------------------------
-- Module      :  HJScript.Events
-- Copyright   :  (c) Joel Björnson 2006
-- License     :  BSD-style
-- Maintainer  :  Joel Björnson, joel.bjornson@gmail.com
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Events 
  (
    -- * Data
    Event(..),
    
    -- * Functions
    showEvent
  ) where

-- import HJScript.Lang

import Data.Char (toLower)


-----------------------------------------------------------
-- HJScript function for adding Events to an element
-----------------------------------------------------------

-- | Events

data Event
  = OnAbort
  | OnBlur
  | OnChange
  | OnClick
  | OnDblclick
  | OnError
  | OnFocus
  | OnKeyDown
  | OnKeyPress
  | OnKeyUp
  | OnLoad
  | OnMouseDown
  | OnMouseMove
  | OnMouseOut
  | OnMouseOver
  | OnMouseUp
  | OnReset
  | OnResize
  | OnSelect
  | OnSubmit
  | OnUnload
 deriving Show

showEvent :: Event -> String
showEvent = map toLower . show

{- What are we using this code for?

-- | Attach an event to an ElementNode
-- Uses either addEventListener or AttachEvent depending on browser..
addEvent :: (IsElementNode n, IsHJScript s) => 
            Event -> s -> JObject n -> HJScript ()

addEvent ev fun elm = do
  fun' <- procedure $ \() -> toHJScript fun
  doIf (hasAddEL)
      (addEventListener (toJsExp $ showEventAddE ev,fun',false) elm) $
    doElse $ doIfNoElse (hasAddEL) 
      (attachEvent (toJsExp $ showEventAddE ev,fun') elm)
  where
    hasAddEL = window `hasFeature` "addEventListener"
    hasAtEL  = window `hasFeature` "attachEvent"


-- | Show event for attache event method (used in I.E.)
showEventAttE :: Event -> String
showEventAttE event = "on" ++ map toLower (show event)

-- | Show event for add event method (used in Mz.)
showEventAddE :: Event -> String
showEventAddE = map toLower . show

-}