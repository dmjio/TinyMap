{-# LANGUAGE BangPatterns #-}

module Data.TinyMap ( TinyMap(..)
                    , lookup
                    , insert
                    , size
                    , delete
                    , empty
                    , map
                    , foldl'
                    , fromList
                    , toList
                    ) where

import           Codec.Compression.Zlib (compress, decompress)
import           Control.Arrow          (second)
import qualified Data.ByteString        as SB
import qualified Data.ByteString.Lazy   as LB
import qualified Data.Foldable          as F
import           Data.Hashable          (Hashable)
import qualified Data.HashMap.Strict    as H
import           Data.Monoid            (Monoid(..))
import           Data.Serialize         (Serialize, decodeLazy, encodeLazy)
import           Prelude                hiding (lookup, map)
import qualified Prelude                as P

newtype TinyMap k v = TinyMap (H.HashMap k LB.ByteString)

-- * lookup by key, if found, decompress and deserialize
lookup :: (Serialize v, Hashable k, Eq k) => k -> TinyMap k v -> Maybe v
lookup k (TinyMap hmap) =
    case H.lookup k hmap of
      Nothing -> Nothing
      Just compressed ->
          case decodeLazy (decompress compressed) of
            Left msg -> error msg
            Right !decoded -> Just decoded
{-# INLINE lookup #-}

-- * insert by key, serialize and compress value
insert :: (Serialize v, Hashable k, Eq k) => k -> v -> TinyMap k v -> TinyMap k v
insert key val (TinyMap map) =
    let serialized = encodeLazy val
        compressed = compress serialized
    in TinyMap $! H.insert key compressed map
{-# INLINE insert #-}

singleton :: (Serialize v, Hashable k, Eq k) => k -> v -> TinyMap k v
singleton key val = insert key val empty
{-# INLINE singleton #-}

size :: (Serialize v, Hashable k, Eq k) => TinyMap k v -> Int
size (TinyMap hmap) = H.size hmap
{-# INLINE size #-}

delete :: (Serialize v, Hashable k, Eq k) => k -> TinyMap k v -> TinyMap k v
delete key (TinyMap hmap) = TinyMap $! H.delete key hmap
{-# INLINE delete #-}

map :: (Serialize v, Serialize v1, Hashable k, Eq k) => (v -> v1) -> TinyMap k v -> TinyMap k v1
map f (TinyMap hmap) = TinyMap . H.fromList . P.map go . H.toList $ hmap
    where go (key, compressed) =
              case decodeLazy (decompress compressed) of
                Left msg -> error msg
                Right !decoded -> (,) key . compress . encodeLazy . f $ decoded
{-# INLINE map #-}

empty :: (Serialize v, Hashable k) => TinyMap k v
empty = TinyMap $! H.empty
{-# INLINE empty #-}

foldl' :: (Serialize v, Hashable k) => (a -> v -> a) -> a -> TinyMap k v -> a
foldl' f acc (TinyMap hmap) = go acc (H.elems hmap)
    where
      go acc [] = acc
      go acc (compressed : xs) =
          case decodeLazy (decompress compressed) of
            Left msg -> error msg
            Right !decoded -> go (let !res = f acc decoded in res) xs
{-# INLINE foldl' #-}

toList :: (Serialize v, Hashable k) => TinyMap k v -> [(k,v)]
toList (TinyMap hmap) = P.map (second f) . H.toList $ hmap
  where f compressed =
            case decodeLazy (decompress compressed) of
              Left msg -> error msg
              Right !decoded -> decoded
{-# INLINE toList #-}

fromList :: (Serialize v, Hashable k, Eq k) => [(k, v)] -> TinyMap k v
fromList xs = go xs empty
  where go [] tmap = tmap
        go ((key,val):xs) tmap = go xs (insert key val tmap)
{-# INLINE fromList #-}

instance (Eq k, Hashable k, Serialize v) => Monoid (TinyMap k v) where
  mempty = empty
  tm1 `mappend` tm2 = F.foldl' step tm1 (toList tm2)
    where step tm (k, v) = insert k v tm
