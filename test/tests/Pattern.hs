module Pattern where

import Data.List.NonEmpty as List1


test = List1.cycle (1 :| [2,3]) 

test2 = 1 :| [2,3,1,2,3]
