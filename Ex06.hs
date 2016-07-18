{-# LANGUAGE GADTs, EmptyDataDecls, TypeFamilies, TypeOperators, DataKinds, FlexibleInstances #-}

module Ex06 where

data List a = Nil | a ::: List a

data Format (fmt :: List *) where
  X :: Format Nil 
  L :: String -> Format fmt -> Format fmt
  S :: Format fmt -> Format (String::: fmt)
  I :: Format fmt ->  Format (Int::: fmt)
 
type family FormatArgsThen (fmt :: List *) (ty :: *) 
type instance FormatArgsThen Nil ty = ty
type instance FormatArgsThen (t ::: fmt) ty = t -> FormatArgsThen fmt ty

printf :: Format fmt -> FormatArgsThen fmt String
printf f = printf' f ""
  where
    printf' :: Format fmt -> String -> FormatArgsThen fmt String 
    printf' X s = s
    printf' (L str fmt) s = printf' fmt (s ++ str ) 
    printf' (S fmt) s = \str->printf' fmt (s ++ str)
    printf' (I fmt) s = \n->printf' fmt (s ++ show n)  



--(L "Hello " (S (L "! You are " (I (L " years old!" X))))) "bob" 19