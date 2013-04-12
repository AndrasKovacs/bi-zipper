{- | 
Simple monad for "Data.Traversable.BiZipper". 

It is just a MaybeT over a BiZipper State that fails with Nothing whenever we walk off the edge of the data structure. 

The semantics of the functions is pretty much the same as in "Data.Traversable.BiZipper".

Sample code:

@
import Data.Traversable.BiZipper
import Control.Monad.BiZipper

foo :: Num a => BiZipperM t () a
foo = do 
    nextM
    nextM
    updateM 300
    nextM
    modifyM (*100)

main = do
    let zipper = BZ.mkBiZipper [1..10]
        Just zipper' = runBiZipperM zipper foo
    print $ BZ.zipUp zipper' -- prints [1, 2, 300, 400, 5, 6, 7, 8, 9, 10]
@
-}

module Control.Monad.BiZipper (
    -- * Types
      BiZipperM
    -- * Functions
    , nextM
    , prevM
    , updateM
    , modifyM
    , isAtEndM
    , isAtStartM
    , valueM
    , runBiZipperM) where

import Control.Monad.State
import Control.Monad.Trans.Maybe
import qualified Data.Traversable.BiZipper as BZ

-- | The Zipper monad with the base Traversable @t@, element type @a@ and monadic return value @b@.
type BiZipperM t b a = MaybeT (State (BZ.BiZipper t a)) b

nextM :: BiZipperM t () a
nextM = maybe (fail "") (lift . put) . BZ.next =<< lift get

prevM :: BiZipperM t () a
prevM = maybe (fail "") (lift . put) . BZ.prev =<< lift get

updateM :: a -> BiZipperM t () a
updateM = lift . modify . BZ.update

modifyM :: (a -> a) -> BiZipperM t () a
modifyM = lift . modify . BZ.modify

isAtEndM :: BiZipperM t Bool a
isAtEndM = return . BZ.isAtEnd =<< lift get

isAtStartM :: BiZipperM t Bool a
isAtStartM = return . BZ.isAtStart =<< lift get

valueM :: BiZipperM t a a
valueM = return . BZ.value =<< lift get

runBiZipperM :: BZ.BiZipper t a -> BiZipperM t b a -> Maybe (BZ.BiZipper t a)
runBiZipperM bz m = case runState (runMaybeT m) bz of
    (Just a, bz) -> Just bz
    (Nothing, _)  -> Nothing
