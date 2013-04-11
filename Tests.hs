
import Test.QuickCheck
import Test.Framework
import Test.Framework.Providers.QuickCheck2

import Data.Traversable.BiZipper
import Control.Monad.BiZipper

import qualified Data.Map as M
import Control.Monad

instance (Ord k, Arbitrary k, Arbitrary v) => Arbitrary (M.Map k v) where
    arbitrary = liftM2 ((M.fromList .) . zip) arbitrary arbitrary

prop_clean_zipup_lst :: [Int] -> Bool
prop_clean_zipup_lst xs = zipUp (mkBiZipper xs) == xs

prop_clean_zipup_map :: M.Map Int Int -> Bool
prop_clean_zipup_map xs = zipUp (mkBiZipper xs) == xs

prop_next_indexing :: [Int] -> Property
prop_next_indexing xs = 
    (not $ null xs) ==> 
    forAll (choose (0, length xs - 1)) $ \i ->
    (xs !! i) == (maybe undefined value $ foldl (>>=) (return $ mkBiZipper xs) (replicate i next))


tests = [ 
    testProperty "zipUp list" prop_clean_zipup_lst,
    testProperty "zipUp map" prop_clean_zipup_map,
    testProperty "next indexing" prop_next_indexing
    ]

main = defaultMain tests











