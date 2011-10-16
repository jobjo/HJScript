-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.DOM.XHTML
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.DOM.XHTML
  (
    -- * Standard properties
    IsXHTMLElement, asXHTMLElement, className, 
    
    -- * XHTML DOM elements
    Anchor(..), Area(..), Base(..), Body(..),
    Event(..), Form(..), Frame(..), Frameset(..), History(..), 
    IFrame(..), Image(..), InputButton(..), InputCheckbox(..), InputFile(..),
    InputHidden(..), InputPassword(..), InputRadio(..), InputReset(..),
    InputSubmit(..), InputText(..), Link(..),Location(..), 
    Meta(..), Navigator(..), Object(..), Option(..), Screen(..), 
    Select(..), Style(..), Table(..), TableData(..), TableHeader(..), 
    TableRow(..), Textarea(..)
  ) where

import HJScript.DOM.NodeTypes
import HJScript.Lang
import HJScript.DOM.Node
import HJScript.DOM.ElementNode
import HJScript.Objects.Object(Object(..))

-----------------------------------
-- Class gathering all standard methods

class IsElementNode a => IsXHTMLElement a

-- General XHTML element type.
data XHTMLElement = XHTMLElement deriving Show
instance IsClass XHTMLElement
instance IsElementNode XHTMLElement
instance IsXHTMLElement XHTMLElement

asXHTMLElement :: IsElementNode n => Exp n -> Exp XHTMLElement
asXHTMLElement = castObject

-- Standard methods

className :: IsXHTMLElement n => Exp n -> Var String
className = derefVar "className"

dir :: IsXHTMLElement n => Exp n -> Var String
dir = derefVar "lang"

lang :: IsXHTMLElement n => Exp n -> Var String
lang = derefVar "lang"

title :: IsXHTMLElement n => Exp n -> Var String
title = derefVar "title"


-- Going to and from ElementNode.
generalize :: IsXHTMLElement a => Exp a -> Exp ElementNode
generalize = castObject
specialize :: IsXHTMLElement a => Exp ElementNode -> Exp a
specialize = castObject



-- A lot more work should be done here, to the point
-- where each separate element should have its own
-- module.

-- Anchor
data Anchor = Anchor deriving Show
instance IsClass Anchor
instance IsNode Anchor
instance IsElementNode Anchor
instance IsXHTMLElement Anchor

-- Area
data Area = Area deriving Show
instance IsNode Area
instance IsClass Area
instance IsElementNode Area
instance IsXHTMLElement Area

-- Base
data Base = Base deriving Show
instance IsClass Base
instance IsNode Base
instance IsElementNode Base
instance IsXHTMLElement Base

-- Body
data Body = Body deriving Show
instance IsClass Body
instance IsNode Body
instance IsElementNode Body
instance IsXHTMLElement Body

-- Event
data Event = Event deriving Show
instance IsClass Event

-- Form
data Form = Form deriving Show
instance IsClass Form
instance IsNode Form
instance IsElementNode Form
instance IsXHTMLElement Form

-- Frame
data Frame = Frame deriving Show
instance IsClass Frame
instance IsNode Frame
instance IsElementNode Frame
instance IsXHTMLElement Frame

-- Frameset
data Frameset = Frameset deriving Show
instance IsClass Frameset

-- History
data History = History deriving Show
instance IsClass History

-- IFrame
data IFrame = IFrame deriving Show
instance IsClass IFrame
instance IsNode IFrame
instance IsElementNode IFrame
instance IsXHTMLElement IFrame

-- Image
data Image = Image deriving Show
instance IsClass Image
instance IsNode Image
instance IsElementNode Image
instance IsXHTMLElement Image

-- InputButton
data InputButton = InputButton deriving Show
instance IsClass InputButton
instance IsNode InputButton
instance IsElementNode InputButton
instance IsXHTMLElement InputButton

-- InputCheckbox
data InputCheckbox = InputCheckbox deriving Show
instance IsClass InputCheckbox
instance IsNode InputCheckbox
instance IsElementNode InputCheckbox
instance IsXHTMLElement InputCheckbox

-- InputFile
data InputFile = InputFile deriving Show
instance IsClass InputFile

-- InputHidden
data InputHidden = InputHidden deriving Show
instance IsClass InputHidden

-- InputPassword
data InputPassword = InputPassword deriving Show
instance IsClass InputPassword

-- InputRadio
data InputRadio = InputRadio deriving Show
instance IsClass InputRadio

-- InputReset
data InputReset = InputTextInputReset deriving Show
instance IsClass InputReset

-- InputSubmit
data InputSubmit = InputSubmit deriving Show
instance IsClass InputSubmit

-- InputText
data InputText = InputText deriving Show
instance IsClass InputText
instance IsNode InputText
instance IsElementNode InputText
instance IsXHTMLElement InputText

-- Link
data Link = Link deriving Show
instance IsClass Link
instance IsNode Link
instance IsElementNode Link
instance IsXHTMLElement Link

-- Location
data Location = Location deriving Show
instance IsClass Location

-- Meta
data Meta = Meta deriving Show
instance IsClass Meta

-- Navigator
data Navigator = Navigator deriving Show
instance IsClass Navigator

-- Option
data Option = Option deriving Show
instance IsClass Option

-- Screen
data Screen = Screen deriving Show
instance IsClass Screen

-- Select
data Select = Select deriving Show
instance IsClass Select

-- Style
data Style = Style deriving Show
instance IsClass Style

-- Table
data Table = Table deriving Show
instance IsClass Table
instance IsNode Table
instance IsElementNode Table
instance IsXHTMLElement Table

-- TableData
data TableData = TableData deriving Show
instance IsClass TableData
instance IsNode TableData
instance IsElementNode TableData
instance IsXHTMLElement TableData

-- TableHeader
data TableHeader = TableHeader deriving Show
instance IsClass TableHeader
instance IsNode TableHeader
instance IsElementNode TableHeader
instance IsXHTMLElement TableHeader

-- TableRow
data TableRow = TableRow deriving Show
instance IsClass TableRow
instance IsNode TableRow
instance IsElementNode TableRow
instance IsXHTMLElement TableRow

-- Textarea
data Textarea = Textarea deriving Show
instance IsClass Textarea
instance IsNode Textarea
instance IsElementNode Textarea
instance IsXHTMLElement Textarea
