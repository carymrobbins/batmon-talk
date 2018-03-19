{-# OPTIONS_GHC -fno-warn-missing-signatures #-}
module Batmon.Prelude
  ( module Batmon.Prelude
  , module X
  ) where

-- Standard re-exports
import Control.Exception as X (Exception, SomeException, throw, try)
import Control.Lens as X ((^.), (.~))
import Control.Monad as X
import Control.Monad.IO.Class as X (MonadIO(liftIO))
import Data.ByteString as X (ByteString)
import Data.Either as X (isLeft, isRight)
import Data.Function as X ((&))
import Data.Functor as X ((<&>))
import Data.Map.Strict as X (Map)
import Data.Maybe as X (fromMaybe, listToMaybe, mapMaybe)
import Data.Proxy as X (Proxy(Proxy))
import Data.String as X (IsString(fromString))
import Data.Text as X (Text)
import Data.UUID as X (UUID)
import GHC.Generics as X (Generic)
import GHC.Exts as X (fromList)
import Prelude as X hiding (error, undefined)

-- Orphan instances
import Batmon.Orphans ()
import Data.Generics.Labels ()

-- Customized re-exports
import qualified Control.Exception
import qualified Data.Maybe
import qualified Prelude

{-# DEPRECATED error "The use of 'error' is discouraged! Use 'throw' instead." #-}
error = Prelude.error

{-# DEPRECATED undefined "The use of 'undefined' is discouraged! Use 'throw' instead." #-}
undefined = Prelude.undefined

unsafeFromJust = Data.Maybe.fromJust

unsafeGetLeft :: Either a b -> a
unsafeGetLeft = \case
  Left a -> a
  Right _ -> throw $ userError "Expected Left, got Right"

unsafeGetRight :: Either a b -> b
unsafeGetRight = \case
  Right b -> b
  Left _ -> throw $ userError "Expected Right, got Left"

-- | Lifted version of 'throwIO' which works in any 'MonadIO'.
throwIO :: (MonadIO m, Exception e) => e -> m a
throwIO = liftIO . Control.Exception.throwIO
