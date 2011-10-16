{-# LANGUAGE OverlappingInstances, UndecidableInstances #-}
module HJScript.Objects.JQuery where

import HJScript.Lang
import HJScript.DOM.Window
import HJScript.DOM.ElementNode

data JQuery = JQuery deriving Show
instance IsClass JQuery


-- | Constructors for Date
instance HasConstructor JQuery JString String

jQuery :: Exp JQuery
jQuery = JConst "jQuery"

selectExpr :: Exp c -> JObject JQuery
selectExpr e = methodCall "jQuery" e window

jSize :: JObject JQuery -> JInt
jSize = methodCallNoArgs "size"

length :: JObject JQuery -> JInt
length = deref "length"

get ::  JInt -> JObject JQuery -> Exp ElementNode
get = methodCall "get"

empty :: JObject JQuery -> Exp JQuery
empty = methodCall "empty" ()

jVal :: JObject JQuery -> JString
jVal = methodCall "val" ()

jSetVal :: JString -> JObject JQuery -> JString
jSetVal = methodCall "val"

jText :: JObject JQuery -> JString
jText = methodCall "text" ()

jSetText :: JString -> JObject JQuery -> Exp JQuery
jSetText = methodCall "text"

append :: Exp a -> JObject JQuery -> Exp JQuery
append = methodCall "append"

prepend :: Exp a -> JObject JQuery -> Exp JQuery
prepend = methodCall "prepend" 

ready :: HJScript () -> HJScript ()
ready script
    = do fn <- procedure $ \() -> script
         runExp $ methodCall "jQuery" fn window

change :: HJScript () -> JObject JQuery -> HJScript ()
change script query
    = do fn <- procedure $ \() -> script
         runExp $ methodCall "change" fn query

submit :: HJScript () -> JObject JQuery -> HJScript ()
submit script query
    = do fn <- procedure $ \() -> script
         runExp $ methodCall "submit" fn query

select :: HJScript () -> JObject JQuery -> HJScript ()
select script query
    = do fn <- procedure $ \() -> script
         runExp $ methodCall "select" fn query

runExp :: Exp a -> HJScript ()
runExp = outputStmt . ExpStmt

