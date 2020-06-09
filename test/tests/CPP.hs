{-# LANGUAGE CPP #-}
module CPP where
#if !(MIN_VERSION_base(4,8,0))
import Data.Monoid
#endif
#if !(MIN_VERSION_base(4,11,0))
import Data.Semigroup
#endif

#if !(MIN_VERSION_base(4,5,0)) && !(MIN_VERSION_base(4,9,0))

infixr 6 <>

-- | An infix synonym for 'mappend'.
--
-- /Since: 4.5.0.0/
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}

#endif

str = "abc" <> "def"
