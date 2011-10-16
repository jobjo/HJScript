-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.DOM.NodeTypes
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.DOM.NodeTypes
  (
    NodeType(..),
    -- Node types
    Node(..), ElementNode(..), AttributeNode(..), TextNode(..),
    -- CDataSectionNode, EntityReferenceNode, EntityNode,
    -- ProcessingInstructionNode, CommentNode, DocumentTypeNode,
    -- DocumentFragmentNode, NotationNode, 
        
    -- Functions
    nodeTypeVal,
    
    -- Document to avoid cycle
    Document(..)
  ) where

import HJScript.Lang

----------------------------------------------------
-- Node types
----------------------------------------------------

-- | Different node types
data NodeType 
  = NodeElement
  | NodeAttribute
  | NodeText
  | NodeCDataSection
  | NodeEntety
  | NodeEntetyRef
  | NodeProccInstr
  | NodeComment
  | NodeDocument
  | NodeDocType
  | NodeDocFrag
  | NodeNotation


-- Maps node type to type value
nodeTypeVal :: NodeType -> JInt 
nodeTypeVal tp = JInt $ case tp of
    NodeElement       -> 1
    NodeAttribute     -> 2
    NodeText          -> 3
    NodeCDataSection  -> 4
    NodeEntety        -> 5
    NodeEntetyRef     -> 6
    NodeProccInstr    -> 7
    NodeComment       -> 8
    NodeDocument      -> 9
    NodeDocType       -> 10
    NodeDocFrag       -> 11
    NodeNotation      -> 12

----------------------------------------------------
-- Classes for shared properties and methods
----------------------------------------------------    
-- Generic Node
data Node = Node deriving Show

-- ElementNode
data ElementNode = ElementNode deriving Show

-- AttributeNode
data AttributeNode = AttributeNode deriving Show

-- TextNode
data TextNode = TextNode deriving Show

{- I see no need to include these at this point.
-- CDataSectionNode
data CDataSectionNode = CDataSectionNode deriving Show

-- EntityReferenceNode
data EntityReferenceNode = EntityReferenceNode deriving Show

-- EntityNode
data EntityNode = EntityNode deriving Show

-- ProcessingInstructionNode
data ProcessingInstructionNode = ProcessingInstructionNode deriving Show

-- CommentNode
data CommentNode = CommentNode deriving Show

-- DocumentTypeNode
data DocumentTypeNode = DocumentTypeNode deriving Show

-- DocumentFragmentNode
data DocumentFragmentNode = DocumentFragmentNode deriving Show

-- NotationNode
data NotationNode = NotationNode deriving Show
-}

-- We need to include this here to avoid a cyclic dependency
data Document = Document deriving Show 