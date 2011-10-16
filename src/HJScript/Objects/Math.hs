{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Objects.Math
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Objects.Math
  (
    Math(..),
    abs,
    acos,
    asin,
    atan,
    cos,
    exp,
    floor,
    log,
    max,
    min,
    pow,
    random,
    round,
    sin,
    sqrt,
    tan

  ) where

import HJScript.Lang
import Prelude hiding ( abs,acos,asin,atan,cos,exp,floor,log,
                        max,min,round,sin,sqrt,tan)

-- | Math Class
data Math = Math deriving Show
instance IsClass Math
instance IsDeref Math

callMathMeth :: Num t => String -> Exp t -> JFloat
callMathMeth name exp = callMethod name exp Math

abs :: Num t => Exp t -> JFloat
abs = callMathMeth "abs"

acos ::  Num t => Exp t -> JFloat
acos = callMathMeth "acos"

asin ::  Num t => Exp t -> JFloat
asin = callMathMeth "asin"

atan :: Num t => Exp t -> JFloat
atan = callMathMeth "atan"

cos ::  Num t => Exp t -> JFloat
cos = callMathMeth "cos"

exp ::  Num t => Exp t -> JFloat
exp = callMathMeth "exp"

floor ::  Num t => Exp t -> JFloat
floor = callMathMeth "floor"

log ::  Num t => Exp t -> JFloat
log = callMathMeth "log"

max ::  Num t => Exp t -> JFloat
max = callMathMeth "max"

min ::  Num t => Exp t -> JFloat
min = callMathMeth "min"

pow ::  Num t => Exp t -> JFloat
pow = callMathMeth "pow"

random ::  Num t => Exp t -> JFloat
random = callMathMeth "random"

round ::  Num t => Exp t -> JFloat
round = callMathMeth "round"

sin ::  Num t => Exp t -> JFloat
sin = callMathMeth "sin"

sqrt ::Num t => Exp t -> JFloat
sqrt = callMathMeth "sqrt"

tan ::  Num t => Exp t -> JFloat
tan = callMathMeth "tan"
