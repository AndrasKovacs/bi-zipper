module Data.Traversable.BiZipper (
      Zipper(..)
    , BiZipper(..)
    , mkZipper
    , mkBiZipper
    , next
    , prev
    , update
    , modify
    , value
    , isDone
    , zipUp) where  

import Control.Monad.Cont
import qualified Data.Traversable as T

data Zipper t a = ZDone (t a) | Zipper a (Maybe a -> Zipper t a)
data BiZipper  t a = BiZipper [Maybe a] [Maybe a] [Zipper t a]

mkZipper :: T.Traversable t => t a -> Zipper t a
mkZipper t = (`runCont` id) $ T.mapM yield t >>= return . ZDone 
    where yield a = cont $ \k -> Zipper a (k . maybe a id)

mkBiZipper :: T.Traversable t => t a -> BiZipper t a
mkBiZipper t = BiZipper [] (repeat Nothing) [mkZipper t]

next :: BiZipper t a -> Maybe (BiZipper t a)
next (BiZipper ps (f:fs) zs@(Zipper _ k:_)) = Just $ BiZipper (f:ps) fs (k f:zs)
next _ = Nothing

prev :: BiZipper t a -> Maybe (BiZipper t a)
prev (BiZipper (p:ps) fs (z:zs)) = Just $ BiZipper ps (p:fs) zs
prev (BiZipper [] _ _) = Nothing

update :: a -> BiZipper t a -> BiZipper t a
update a (BiZipper ps (_:fs) zs) = BiZipper ps (Just a:fs) zs

value :: BiZipper t a -> a
value (BiZipper _ (f:_) (Zipper a _:_)) = maybe a id f

modify :: (a -> a) -> BiZipper t a -> BiZipper t a
modify g (BiZipper ps (f:fs) zs@(Zipper a _:_)) = BiZipper ps (Just f':fs) zs
    where f' = maybe (g a) g f

zipUp :: BiZipper t a -> t a
zipUp (BiZipper _ fs (z:_)) = go z fs where
    go (Zipper _ k) (f:fs) = go (k f) fs
    go (ZDone a) _ = a

isDone :: BiZipper t a -> Bool
isDone (BiZipper _ _ (ZDone _:_)) = True
isDone _ = False