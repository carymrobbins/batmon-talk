module Batmon.Free where

import Control.Compose ((:.))
import Control.Natural (type (~>))
import Prelude

import qualified Control.Applicative.Free as FreeAp
import qualified Control.Compose as Compose

-- | Same as 'Free.runAp' except supports composed higher-kinded
-- type variables.
runApComposed
  :: forall f g h a. (Applicative g, Applicative h)
  => (forall x. f x -> g (h x))
  -> FreeAp.Ap f a
  -> g (h a)
runApComposed f fa = Compose.unO $ FreeAp.runAp f' fa
  where
  f' :: f ~> g :. h
  f' = Compose.O . f
