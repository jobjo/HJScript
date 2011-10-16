{-# LANGUAGE FlexibleInstances, OverlappingInstances, TypeSynonymInstances, UndecidableInstances #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  HJScript.Lang
-- License     :  BSD-style
-- Maintainer  :  Joel Bjornson joel.bjornson@gmail.com,
--                Niklas Broberg nibro@cs.chalmers.se
-- Stability   :  experimental
-----------------------------------------------------------------------------
module HJScript.Lang
  (
    -- Operators
    preinc, postinc, predec, postdec,
    (.+.), (.-.), (.*.), (./.), (.&&.), (.||.), (.==.), (.!=.),
    (.>.), (.<.) , (.>=.), (.<=.) , (.=.), (.+=.), (?), (<|>),

    -- Method calls
    this, callMethod, callVoidMethod, callProc,
    
    -- Functions and declarations
    function, procedure, functionDecl, procedureDecl,
    
    -- Control flow
    for, forIn, forInVar, while, doWhile, doIf, doElse, doIfElse, doIfNoElse, noElse,
    
    -- Objects
    var, varWith, inVar, new, delete, ( # ), ( #. ),
    rec, first, second, x, y,
    
    -- Helpers    
    ( #! ) , jnull, jShow, castObject, hasFeature, 
    break, continue, true ,ifOp, false, int, float, bool, string,

    -- Re-exports from internal module HJScript.Monad
    HJScript, IsHJScript(..), 
    outputBlock, outputStmt,
    
    -- Evaluating HJScript
    evaluateHJScript, evalHJScript,

    -- Re-export all of Language.HJavaScript.Syntax
    module Language.HJavaScript.Syntax

  )  where

import Language.HJavaScript.Syntax
import HJScript.Monad
import Prelude hiding (break)


-- Infix operators
infixr  2   .||. 
infixr  3   .&&.
infix   4   .=. , .==. , .!=., .>., .<. , .<=. , .>=. , ? , 
            `doIfNoElse` , `doIfElse`
infixl  6   .+. , .-.
infixl  7   .*., ./.
infixl  8   # , #! , #. , <|>

-------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------
type HJSJBinOperator t r = Exp t -> Exp t -> Exp r
                        
-- | Incrementing or decrementing numbers.
preinc :: Num t => Var t -> HJScript ()
preinc = outputStmt . ExpStmt . JIncrement Pre

postinc :: Num t => Var t -> HJScript ()
postinc = outputStmt . ExpStmt . JIncrement Pst

predec :: Num t => Var t -> HJScript () 
predec = outputStmt . ExpStmt . JDecrement Pre

postdec :: Num t => Var t -> HJScript ()
postdec = outputStmt . ExpStmt . JDecrement Pst

binOp :: BinOp t r -> HJSJBinOperator t r
binOp op e1 e2 = JBinOp (toExp e1) op (toExp e2)

(.+.) :: PlusOpType a => HJSJBinOperator a a
(.+.) = binOp Plus

(.-.) :: Num a => HJSJBinOperator a a
(.-.) = binOp Minus

(.*.) :: Num a => HJSJBinOperator a a
(.*.) = binOp Times

(./.) :: Num a => HJSJBinOperator a a
(./.) = binOp Div

(.&&.) :: HJSJBinOperator Bool Bool
(.&&.) = binOp And

(.||.) :: HJSJBinOperator Bool Bool
(.||.) = binOp Or

(.==.) :: HJSJBinOperator a Bool
(.==.) = binOp Equals 

(.!=.) :: HJSJBinOperator a Bool
(.!=.) = binOp NotEquals 

(.>.) :: Num a => HJSJBinOperator a Bool
(.>.) = binOp GThan

(.<.) :: Num a => HJSJBinOperator a Bool
(.<.) = binOp LThan

(.>=.) :: Num a => HJSJBinOperator a Bool
(.>=.) = binOp GEThan

(.<=.) :: Num a => HJSJBinOperator a Bool
(.<=.) = binOp LEThan

-- | Assignment
(.=.) ::  Var t -> Exp t -> HJScript ()
v .=. e = outputStmt . ExpStmt $ JAssign v e

-- | Plus with
(.+=.) :: Num t => Var t -> Exp t -> HJScript ()
v .+=. e = outputStmt . ExpStmt $ JAssignWith v PlusAssign e

-----------------------------------------------------------
-- Control flow
-----------------------------------------------------------

-- | for
for :: JInt -> JInt -> (JInt -> HJScript t) -> HJScript ()
for from to script = do
  name <- newVarName
  (_,body) <- hjsInside $ script (val $ JVar name)
  outputStmt $ For (pre name) (cond name) (inc name) body
  where
    inc name  = JIncrement Pst (JVar name)  :: JInt
    pre name  = VarDeclAssign name from
    cond name = (val $ JVar name) .<=. to
    
-- | for (var in object) { .. }    
forIn :: (IsDeref d) => d -> (JString -> HJScript ()) -> HJScript ()
forIn obj script =
  do v <- var
     (_, body) <- hjsInside $ script (val v)
     outputStmt $ ForIn v obj body
     
-- | for (var in object) { .. }    
forInVar :: (IsDeref d) => d -> (Var a -> HJScript ()) -> HJScript ()
forInVar obj script =
  do v <- var
     (_, body) <- hjsInside $ script (obj # propertyVar (val v))
     outputStmt $ ForIn v obj body     
     
-- | while
while :: JBool -> HJScript t -> HJScript ()
while cond script = do
  (_,body) <- hjsInside script
  outputStmt $ While cond body

-- | doWhile
doWhile :: HJScript t -> JBool -> HJScript ()
doWhile = flip while

-- | doIf
doIf :: JBool -> HJScript t -> HJScript (Elses ()) -> HJScript ()
doIf cond script els = do
  (_,body) <- hjsInside script
  els' <- els
  outputStmt $ If cond body els'
  
-- | doElse
doElse :: HJScript () -> HJScript (Elses ())
doElse script = do
  (_,body) <- hjsInside script
  return $ Else body

-- | doIfElse
doIfElse :: JBool -> (HJScript t1, HJScript t2) -> HJScript ()
doIfElse cond (hj1,hj2) = do
  (_,body1) <- hjsInside hj1
  (_,body2) <- hjsInside hj2
  outputStmt $ If cond body1 (Else body2)

-- | Alternative if-else syntax: isTrue ? (doA,doB)
(?) :: JBool -> (HJScript t1, HJScript t2) -> HJScript ()
(?) = doIfElse

-- | Providing a way of writing if-else expression as in: isTrue ? doA <|> doB
(<|>) :: a -> a -> (a,a)
(<|>) = (,)

-- | Only an if branch
doIfNoElse :: Exp Bool -> HJScript () -> HJScript ()
doIfNoElse cond script = doIf cond script noElse    

-- | No else branch.
noElse :: HJScript (Elses ())
noElse = return NoElse

-----------------------------------------------------------
-- HJScript function declarations
-----------------------------------------------------------

-- | Anonymous function, returning an expression
function :: (FormalParams a t, VarsToExps a e) =>
            (e -> HJScript (Exp r)) ->  HJScript (Exp (t -> r))
function fun = do 
  n <- newVarNum
  let args = mkFParams (\_ -> ()) n
  let script = fun $ v2e args
  (ret, body) <-  hjsInside script
  let body' = addReturn ret body
  return $ JFunction Nothing args body'

-- | Anonymous void function.
procedure :: (FormalParams a t, VarsToExps a e) =>
             (e -> HJScript ()) -> HJScript (Exp (t -> ()))
procedure fun = do
  n <- newVarNum
  let args = mkFParams (\_ -> ()) n
  body <- return . snd =<< (hjsInside $ fun $ v2e args)
  return $ JFunction Nothing args body

-- | Function declaration
functionDecl :: (FormalParams a t, VarsToExps a e) =>
                String -> (e -> HJScript (Exp r)) -> HJScript ()
functionDecl name fun = do
  n <- newVarNum
  let args = mkFParams (\_ -> ()) n
  let script = fun $ v2e args
  (ret,body) <- hjsInside script
  let body' = addReturn ret body
  outputStmt $ ExpStmt $ JFunction (Just name) args body'
   
-- | Procedure declaration.
procedureDecl ::  (FormalParams a t, VarsToExps a e) =>
                  String -> (e -> HJScript ()) -> HJScript ()

procedureDecl name fun = do
  n <- newVarNum
  let args = mkFParams (\_ -> ()) n
  let script = fun $ v2e args
  (_, body) <- hjsInside script
  outputStmt $ ExpStmt $ JFunction (Just name) args body

-- | Adds a return statement to a Block.
addReturn :: Exp t -> Block () -> Block t
addReturn e block = Sequence block (Return e)



-----------------------------------------------------------
-- A return-adding evaluator
-----------------------------------------------------------

evaluateHJScript :: HJScript (Exp t) -> Block t
evaluateHJScript m = 
    let (v,b) = evalHJScript m
     in addReturn v b

-----------------------------------------------------------
-- HJScript method calls
-----------------------------------------------------------

-- Call an object method, returning an expression.
callMethod :: (IsDeref d, Args e t1) => String -> e -> d -> Exp t2
callMethod = methodCall

-- Method call for void methods. Returns a HJScript () since the return value is
-- not of any interest.
callVoidMethod :: (Args e t1, IsDeref a) => String -> e -> a -> HJScript ()
callVoidMethod fun args = outputStmt . ExpStmt . callMethod fun args

-----------------------------------------------------------
-- Variables, objects and records
-----------------------------------------------------------
-- Creates a JavaScript variable with a fresh name.
var :: HJScript (Var t)
var = do
  name <- newVarName
  outputStmt $ VarDecl name
  return $ JVar name

-- Assign an expression to a new variable.
varWith :: Exp t -> HJScript (Var t)
varWith e = do
  name <- newVarName
  outputStmt $ VarDeclAssign name e
  return $ JVar name 

inVar :: Exp t -> HJScript (Exp t)
inVar = fmap val . varWith

this :: IsClass c => Exp c
this = JThis

callProc :: (Args e t) => Exp (t -> t1) -> e -> HJScript ()
callProc e = outputStmt . ExpStmt . (JCall e)

-- Create new Objects.
new ::  (HasConstructor o e t, Args e t) => o -> e -> HJScript (Exp o)
new o = fmap val . varWith . JNew o                     

-- |delete a property
--
-- Can only delete properties/variables that are created implicitly,
-- not those declared with the var statement.
--
-- returns true if property was deleted. false if operation was not possible.
delete :: Var a -> Exp Bool
delete = JDelete

-- | Dereferencing operator, similar to the `dot` operator in JavaScript.
-- E.g. document.forms => document # forms, same as forms document
( # ) :: a -> (a -> b) -> b
a # f = f a

-- Operator used for binding dereferencing without argument, 
-- e.g. "style #. display"
( #. ) :: (a -> b) -> (b -> c) -> (a -> c)
( #. ) = flip (.) 

-- Creating a record
rec :: Exp a -> Exp b -> Exp (Rec a b)
rec = JRec

first, x :: Exp (Rec a b) -> Exp a
first = JFst
x = first

second, y :: Exp (Rec a b) -> Exp b
second = JSnd
y = second

-----------------------------------------------------------
-- Helpers
-----------------------------------------------------------

-- | Accessing arrays.
( #! ) ::  JArray t -> JInt -> Var t
( #! ) = JArrayIndex

-- | Null value
jnull :: IsNullable t => Exp t
jnull = JNull

-- | Converts to JString expression.
jShow :: JShow t => Exp t -> JString
jShow = JShow

-- | Casting an JObject
castObject :: (IsClass c1, IsClass c2) => JObject c1 -> JObject c2
castObject = JCastObject

-- | Checks if an object is supported by browser
hasFeature :: (IsFeature f , IsClass c) => JObject c -> f -> JBool
hasFeature =  JIsImpl

ifOp :: JBool -> Exp t -> Exp t -> Exp t
ifOp = JIfOp

break :: HJScript ()
break = outputStmt Break

continue :: HJScript ()
continue = outputStmt Continue

true :: JBool
true = JBool True

false :: JBool
false = JBool False

int :: Int -> JInt
int = JInt

float :: Float -> JFloat
float = JFloat

bool :: Bool -> JBool
bool = JBool

string :: String -> JString
string = JString
