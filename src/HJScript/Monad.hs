{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances, UndecidableInstances, GADTs #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Monad
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Monad
  (
    -- * Data types and classes
    HJScript, HJScript',
    IsHJScript(..),
    
    -- * Functions
    evalHJScript,
    runHJScript,
    outputBlock,
    outputStmt,
    newVarName,
    newVarNum,
    hjsInside,

  )  where
  
import Language.HJavaScript.Syntax
import Control.Monad.Writer
import Control.Monad.State

import HSX.XMLGenerator (XMLGenT, unXMLGenT)

-- | HJScript Monad
type HJScript'= StateT HJState (Writer (Block ()))
type HJScript = XMLGenT HJScript'

-- | To keep track of number of created variables
type HJState = Int

-- | Init state
initState :: HJState
initState = 0

-- | Shows a HJScript ()
instance Show (HJScript ()) where
  show script = show . snd $ evalHJScript script

-- | Block as a Monoid
instance Monoid (Block ()) where
  mempty = EmptyBlock
  mappend EmptyBlock b = b
  mappend b EmptyBlock = b
  mappend b1 (Sequence b2 s) = Sequence (mappend b1 b2) s  
  
-- | Evaluate a script returning a tuple of the produced value and
-- a block of code.
evalHJScript :: HJScript t -> (t, Block ())
evalHJScript m = runWriter $ evalStateT (unXMLGenT m) initState

-- | Runs a script returning the value, the new state and 
-- the block of code.
runHJScript :: HJScript t -> HJState -> (t, HJState, Block ()) 
runHJScript m state = 
  let ((v,state'),block) = runWriter $ runStateT (unXMLGenT m) state
  in  (v,state',block)

-- Get the state
getHJState :: HJScript HJState
getHJState = lift get

-- Set the state
putHJState :: HJState -> HJScript ()
putHJState = lift . put
  
-- | Adds a statement
outputStmt :: Stmt () -> HJScript ()
outputStmt = outputBlock . toBlock

-- | Adds a block
outputBlock :: Block () -> HJScript ()
outputBlock = lift . lift . tell

-- Creates a fresh variable number
newVarNum :: HJScript Int
newVarNum = lift $ do
  n <- get
  put $ n + 1
  return n

-- Creates a fresh variable name
newVarName :: HJScript String
newVarName = do
  n <- newVarNum
  return $ "var" ++ "_" ++ (show n)

-- | Runs one script inside another
hjsInside :: HJScript t -> HJScript (t, Block ())
hjsInside script = do
  state <- getHJState
  let (v,state',block) = runHJScript script state 
  putHJState state'
  return (v,block)

-------------------------------------------------------------------
-- IsHJScript
-------------------------------------------------------------------
-- | IsHJscript class with function toHJScript for converting 
-- instances to HJScript ()
class IsHJScript a where
  toHJScript :: a -> HJScript ()
  
instance IsHJScript (HJScript t) where
  toHJScript s = s >> return ()  

instance IsHJScript (Block ()) where
  toHJScript = outputBlock

instance IsHJScript (Stmt ()) where
  toHJScript = outputStmt

instance IsHJScript (Exp t) where
  toHJScript = toHJScript . ExpStmt  
