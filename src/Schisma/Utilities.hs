module Schisma.Utilities
  ( foldlWithIndex
  , head
  , inputOutputTypes
  , interleave
  , listDifference
  , listIntersect
  , listUnion
  , mapWithIndex
  , merge
  , mergeLeft
  , renameKeyInMap
  , renameKeysFromMap
  , rotate
  ) where

import           Prelude                 hiding ( head
                                                , lookup
                                                )

import           Data.List                      ( foldl'
                                                , transpose
                                                )
import qualified Data.Map.Merge.Strict         as Map
                                                ( merge
                                                , preserveMissing
                                                , zipWithMatched
                                                )
import           Data.Map.Strict                ( Map
                                                , foldlWithKey'
                                                , fromList
                                                , fromListWith
                                                , insert
                                                , lookup
                                                , updateLookupWithKey
                                                )
import           Data.Set                       ( Set
                                                , fromList
                                                , member
                                                , notMember
                                                )
import           Data.Typeable                  ( Typeable
                                                , typeOf
                                                )

import           Data.Containers.ListUtils      ( nubOrd )
import           Data.Text                      ( Text
                                                , pack
                                                , splitOn
                                                )

-- | Folds over a Foldable type, providing the index of each element with each
--   iteration.
--
-- ==== __Examples__
--
-- >>> foldlWithIndex (\acc idx val -> acc + idx + val) 0 [10, 11, 12]
-- 36
foldlWithIndex
  :: Foldable t1
  => (t2 -> Int -> a -> t2) -- ^ @f@ - The function to apply to each element of
                            --   @xs@.
  -> t2                     -- ^ @z@ - The starting accumulator value.
  -> t1 a                   -- ^ @xs@ - The foldable collection to fold over.
  -> t2                     -- ^ The folded result.
foldlWithIndex f z xs =
  foldl' (\g x i -> f (g (i - 1)) i x) (const z) xs (length xs - 1)

-- | Retrieves the first element in a list, or Nothing if the list is
--   empty.
--
-- ==== __Examples__
--
-- >>> head [10, 11, 12]
-- Just 10
--
-- >>> head []
-- Nothing
head :: (Foldable f) => f a -> Maybe a
head = foldr (\x _ -> pure x) Nothing

-- | Introspects the type signature of a value and splits it into its
-- inputs and output.
--
-- ==== __Examples__
--
-- >>> inputOutputTypes (+)
-- (["Integer", "Integer"], "Integer")
inputOutputTypes
  :: Typeable a
  => a              -- ^ The value.
  -> ([Text], Text) -- ^ The inputs and output.
inputOutputTypes f = parseTypes types where
  types = splitOn " -> " $ pack $ show $ typeOf f
  parseTypes []  = ([], "")
  parseTypes [x] = ([], x)
  parseTypes x   = (init x, last x)

-- | Interleaves two lists.
--
-- ==== __Examples__
--
-- >>> interleave [1, 2] [3, 4]
-- [1, 3, 2, 4]
--
-- >>> interleave [1, 2, 4] [3]
-- [1, 3, 2, 4]
interleave
  :: [a] -- ^ @xs@ - The first list.
  -> [a] -- ^ @ys@ - The second list.
  -> [a] -- ^ The interleaved list.
interleave xs ys = concat (transpose [xs, ys])

-- | Performs a list difference.
--
--   Adapted from https://github.com/nh2/haskell-ordnub.
--
-- ==== __Examples__
--
-- >>> listDifference [1, 1, 2, 3] [1, 3]
-- [1, 2]
listDifference
  :: (Ord a)
  => [a] -- ^ @a@ - The first list.
  -> [a] -- ^ @b@ - The second list.
  -> [a] -- ^ The list @a@ with elements from @b@ removed.
listDifference a b = go initHist a
 where
  initHist = fromListWith (+) [ (x, 1 :: Int) | x <- b ]

  go _    []       = []
  go hist (x : xs) = case lookup x hist of
    Just n | n > 0 -> go (insert x (n - 1) hist) xs
    _              -> x : go hist xs

-- | Performs a list intersection.
--
--   Adapted from https://github.com/nh2/haskell-ordnub.
--
-- ==== __Examples__
--
-- >>> listInstersect [1, 2] [2, 3]
-- [2]
listIntersect
  :: (Ord a)
  => [a] -- ^ @a@ - The first list.
  -> [a] -- ^ @b@ - The second list.
  -> [a] -- ^ The list containing elements commong to @a@ and @b@.
listIntersect a b = filter (`member` bSet) a where bSet = Data.Set.fromList b

-- | Performs a list union.
--
--   Adapted from https://github.com/nh2/haskell-ordnub.
--
-- ==== __Examples__
--
-- >>> listUnion [1, 2, 1] [3, 4]
-- [1, 2, 1, 3, 4]
listUnion
  :: (Ord a)
  => [a] -- ^ @a@ - The first list.
  -> [a] -- ^ @b@ - The second list.
  -> [a] -- ^ The list @a@ with elements from @b@ added.
listUnion a b = a ++ nubOrd (filter (`notMember` aSet) b)
  where aSet = Data.Set.fromList a

-- | Maps over a list, providing the index of each element with each
--   iteration.
--
-- ==== __Examples__
--
-- >>> mapWithIndex (\x i -> i + 5) [0, 1, 2]
-- [5, 6, 7]
mapWithIndex
  :: (a -> Integer -> b) -- ^ @f@ - The function to apply to each element of
                         --   @xs@.
  -> [a]                 -- ^ @xs@ - The list to map over.
  -> [b]                 -- ^ The mapped list.
mapWithIndex f xs = zipWith f xs [0 ..]

-- | Merges two maps. If a key exists in both map @left@ and map @right@, the
--   value corresponding to the key in map @left@ is overwritten with the
--   value from map @right@.
--
-- ==== __Examples__
--
-- >>> merge (fromList [("a", 1), ("b", 2)]) (fromList [("a", 3), ("c", 4)])
-- fromList [("a", 3), ("b", 2), ("c", 4)]
merge
  :: Ord k
  => Map k v -- ^ @left@ - The left map.
  -> Map k v -- ^ @right@ -  The right map.
  -> Map k v -- ^ The merged map.
merge = Map.merge Map.preserveMissing
                  Map.preserveMissing
                  (Map.zipWithMatched f)
  where f _ _ y = y

-- | Merges two maps. If a key exists in both map @left@ and map @right@, the
--   value corresponding to the key in map @left@ is preserved.
--
-- ==== __Examples__
--
-- >>> mergeLeft (fromList [("a", 1), ("b", 2)]) (fromList [("a", 3), ("c", 4)])
-- fromList [("a", 1), ("b", 2), ("c", 4)]
mergeLeft
  :: Ord k
  => Map k v -- ^ @left@ - The left map.
  -> Map k v -- ^ @right@ -  The right map.
  -> Map k v -- ^ The merged map.
mergeLeft = Map.merge Map.preserveMissing
                      Map.preserveMissing
                      (Map.zipWithMatched f)
  where f _ x _ = x

-- | Renames a key in a map.
--
-- ==== __Examples__
--
-- >>> renameKeyInMap (fromList [("a", 1)]) "a" "apple"
-- fromList [("apple", 1)]
renameKeyInMap
  :: Ord k
  => Map k v -- ^ @originalMap@ - The original map.
  -> k       -- ^ @oldKey@ - The old key in the @originalMap@.
  -> k       -- ^ @newKey@ - The new key.
  -> Map k v -- ^ The renamed map.
renameKeyInMap originalMap oldKey newKey =
  case updateLookupWithKey (\_ _ -> Nothing) oldKey originalMap of
    (Nothing   , _     ) -> originalMap
    (Just value, newMap) -> insert newKey value newMap

-- | Renames the keys in a map.
--
-- ==== __Examples__
--
-- >>> renameKeysFromMap (fromList [("a", "apple")]) (fromList [("a", "fruit")])
-- fromList [("apple", "fruit")]
renameKeysFromMap
  :: Ord k
  => Map k k -- ^ @mapWithKeys@ - The map containing the key remappings.
  -> Map k v -- ^ @originalMap@ - The original map.
  -> Map k v -- ^ The map with the renamed keys.
renameKeysFromMap mapWithKeys originalMap =
  foldlWithKey' renameKeyInMap originalMap mapWithKeys

-- | Rotates a list.
--
-- ==== __Examples__
--
-- >>> rotate 1 [1, 2, 3, 4]
-- [2, 3, 4, 1]
--
-- >>> rotate 3 [1, 2, 3, 4]
-- [4, 1, 2, 3]
rotate
  :: Int -- ^ @times@ - The number of times to rotate the list.
  -> [a] -- ^ @xs@ - The list to rotate.
  -> [a] -- ^ The rotated list.
rotate = drop <> take
