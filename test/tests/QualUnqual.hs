module QualUnqual where

import qualified Data.Char as C (isDigit, isLetter)
import Data.Char (isDigit, isLetter)

test x = C.isDigit x || isDigit x
