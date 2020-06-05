module Class where


data Record a = Record { aa :: a,  b :: String }

data Impossible

  = Impossible  String Integer
    -- ^ We reached a program point which should be unreachable.

  | Unreachable String Integer
    -- ^ @Impossible@ with a different error message.
    --   Used when we reach a program point which can in principle
    --   be reached, but not for a certain run.

  | ImpMissingDefinitions [String] String
    -- ^ We reached a program point without all the required
    -- primitives or BUILTIN to proceed forward.
    -- @ImpMissingDefinitions neededDefs forThis@


class CatchImpossible m where

  -- | Catch any 'Impossible' exception.
  catchImpossible :: m a -> (Impossible -> m a) -> m a
  catchImpossible = catchImpossibleJust Just

  -- | Catch only 'Impossible' exceptions selected by the filter.
  catchImpossibleJust :: (Impossible -> Maybe b) -> m a -> (b -> m a) -> m a
  catchImpossibleJust = flip . handleImpossibleJust

  -- | Version of 'catchImpossible' with argument order suiting short handlers.
  handleImpossible :: (Impossible -> m a) -> m a -> m a
  handleImpossible = flip catchImpossible

  -- | Version of 'catchImpossibleJust' with argument order suiting short handlers.
  handleImpossibleJust :: (Impossible -> Maybe b) -> (b -> m a) -> m a -> m a
  handleImpossibleJust = flip . catchImpossibleJust

  {-# MINIMAL catchImpossibleJust | handleImpossibleJust #-}


