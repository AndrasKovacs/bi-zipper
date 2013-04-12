
module Data.Traversable.BiZipper (
    -- * Data 
      BiZipper(..)
    -- * Functions
    , mkBiZipper
    , next
    , prev
    , update
    , modify
    , value
    , isAtStart
    , isAtEnd
    , zipUp) where  

import Control.Monad.Cont
import qualified Data.Traversable as T

data Zipper t a = ZDone (t a) | Zipper a (Maybe a -> Zipper t a)

data BiZipper  t a = BiZipper {
    leftEdits :: [Maybe a],         -- Edits made to the left from the cursor.
    rightEdits :: [Maybe a],        -- Edits made to the right from the cursor.
    leftZippers :: [Zipper t a]     -- Saved zippers to the left from the cursor.
}


mkZipper :: T.Traversable t => t a -> Zipper t a
mkZipper t = (`runCont` id) $ T.mapM yield t >>= return . ZDone 
    where yield a = cont $ \k -> Zipper a (k . maybe a id)


-- | Creates a zipper from any traversable. The focus is initially on the first element.
mkBiZipper :: T.Traversable t => t a -> BiZipper t a
mkBiZipper t = BiZipper [] (repeat Nothing) [mkZipper t]

-- | Advance the focus. Returns Nothing on walking past the end.
next :: BiZipper t a -> Maybe (BiZipper t a)
next (BiZipper ps (f:fs) zs@(Zipper _ k:_)) = Just $ BiZipper (f:ps) fs (k f:zs)
next _ = Nothing


-- | Move the focus backwards. Returns Nothing on walking past the start.  
prev :: BiZipper t a -> Maybe (BiZipper t a)
prev (BiZipper (p:ps) fs (z:zs)) = Just $ BiZipper ps (p:fs) zs
prev _ = Nothing

-- | Replace the current value at the cursor with a new one.
update :: a -> BiZipper t a -> BiZipper t a
update a (BiZipper ps (_:fs) zs) = BiZipper ps (Just a:fs) zs

-- | Returns the current element. 
value :: BiZipper t a -> a
value (BiZipper _ (f:_) (Zipper a _:_)) = maybe a id f

-- | Apply a function to the current value at cursor. 
modify :: (a -> a) -> BiZipper t a -> BiZipper t a
modify g (BiZipper ps (f:fs) zs@(Zipper a _:_)) = BiZipper ps (Just f':fs) zs
    where f' = maybe (g a) g f

-- | Convert a zipper back to the data structure. 
zipUp :: BiZipper t a -> t a
zipUp (BiZipper _ fs (z:_)) = go z fs where
    go (Zipper _ k) (f:fs) = go (k f) fs
    go (ZDone a) _ = a

-- | Returns true if the cursor is at the first element.
isAtStart :: BiZipper t a -> Bool
isAtStart (BiZipper [] _ _) = True
isAtStart _ = True 

-- | Returns true if the cursor is /one past/ the last element.
isAtEnd:: BiZipper t a -> Bool
isAtEnd (BiZipper _ _ (ZDone _:_)) = True
isAtEnd _ = False