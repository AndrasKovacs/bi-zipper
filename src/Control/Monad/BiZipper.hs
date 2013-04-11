module Control.Monad.BiZipper (
      ZipMonad
    , nextM
    , prevM
    , updateM
    , modifyM
    , isDoneM
    , valueM
    , runZipMonad) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Traversable.BiZipper as BZ

type ZipMonad t b a = MaybeT (State (BZ.BiZipper t a)) b

nextM :: ZipMonad t () a
nextM = maybe (fail "") (lift . put) . BZ.next =<< lift get

prevM :: ZipMonad t () a
prevM = maybe (fail "") (lift . put) . BZ.prev =<< lift get

updateM :: a -> ZipMonad t () a
updateM = lift . modify . BZ.update

modifyM :: (a -> a) -> ZipMonad t () a
modifyM = lift . modify . BZ.modify

isDoneM :: ZipMonad t Bool a
isDoneM = return . BZ.isDone =<< lift get

valueM :: ZipMonad t a a
valueM = return . BZ.value =<< lift get

runZipMonad :: BZ.BiZipper t a -> ZipMonad t b a -> Maybe (BZ.BiZipper t a)
runZipMonad bz m = case runState (runMaybeT m) bz of
    (Just a, bz) -> Just bz
    (Nothing, _)  -> Nothing


