{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Objects.Array
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Objects.Array
  (
    -- Properties
    arrLength, 
    
    -- Methods
    push, headArr, mapArray, mapArrayH, mapArrayH_, foreach, filterArray
    
  ) where
  
import HJScript.Lang

-- | Constructors for Array
instance HasConstructor (Array t) () ()
instance HasConstructor (Array t) (JInt) Int

headArr :: JArray t -> Var t
headArr arr = arr #! (int 0)

-- | Properties for Array
arrLength ::  JArray t -> JInt
arrLength = deref "length"

-- | Methods on array
push :: Exp t -> JArray t -> HJScript ()
push arg = callVoidMethod "push"  arg

-- | Map array
mapArray :: (Exp t1 -> Exp t2) ->  JArray t1 ->  HJScript (JArray t2)
mapArray fun arr = do
  retArr <- new Array ()
  for (int 0) (arrLength arr .-. (int 1)) $ \index -> do
    let elem = fun $ val (arr #! index)
    retArr # push elem
  return retArr  

-- | mapArrayH
mapArrayH ::  (Exp t1 -> HJScript (Exp t2)) ->  
              JArray t1 ->  
              HJScript (JArray t2)
mapArrayH fun arr = do
  retArr <- new Array ()
  for (int 0) (arrLength arr .-. (int 1)) $ \index -> do
    elem <- fun $ val (arr #! index)
    retArr # push elem 
  return retArr   
  
-- | Throw away produced value.
mapArrayH_ ::  (Exp t1 -> HJScript t2) ->  JArray t1 ->  HJScript ()
mapArrayH_ fun arr = 
  for (int 0) (arrLength arr .-. (int 1)) $  \index ->  fun $ val (arr #! index)

-- | Synonym for mapArrayH_
foreach :: JArray t1 ->  (Exp t1 -> HJScript t2) ->  HJScript ()
foreach = flip mapArrayH_

-- | Select array elements with a condition.
filterArray :: (Exp t -> JBool) -> JArray t -> HJScript (JArray t)
filterArray cond elems = do
  arr <- new Array ()
  foreach elems $ maybeAdd arr
  return arr
  where
    maybeAdd arr elem = 
      doIfNoElse (elem # cond) $ 
        arr # push elem
