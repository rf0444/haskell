module SimpleJSON (
 JValue(..), getString, getInt, getDouble, getBool, getObject, getArray
 ) where

data JValue
 = JNull
 | JString String
 | JNumber Double
 | JBool Bool
 | JObject [(String, JValue)]
 | JArray [JValue]
 deriving (Eq, Ord, Show)

getString (JString s) = Just s
getString _ = Nothing
getInt (JNumber n) = Just (truncate n)
getInt _ = Nothing
getDouble (JNumber n) = Just n
getDouble _ = Nothing
getBool (JBool b) = Just b
getBool _ = Nothing
getObject (JObject o) = Just o
getObject _ = Nothing
getArray (JArray s) = Just s
getArray _ = Nothing

