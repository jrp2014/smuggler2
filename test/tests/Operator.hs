module Operator where

infixl 5 $+$

($+$) :: [a] -> [a] -> [a]
[] $+$ d = d
d $+$ [] = d
d $+$ d' = d ++ d'
