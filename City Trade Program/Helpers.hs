module Helpers (
get1st,
get2nd,
get3rd
) where

get1st :: (a, b, c) -> a
get1st (x,_, _) = x

get2nd :: (a, b, c) -> b
get2nd (_,x,_) = x

get3rd :: (a, b, c) -> c
get3rd (_,_,x) = x
