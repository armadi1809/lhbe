module Data.AssocMap
  ( AssocMap,
    member,
    alter,
    empty,
    delete,
    insert,
    Data.AssocMap.lookup,
    findWithDefault,
  )
where

newtype AssocMap k v = AssocMap [(k, v)]
  deriving (Show)

empty :: AssocMap k v
empty = AssocMap []

delete :: (Eq k) => k -> AssocMap k v -> AssocMap k v
delete = alter (const Nothing)

insert :: (Eq k) => k -> v -> AssocMap k v -> AssocMap k v
insert key value = alter (const (Just value)) key

lookup :: (Eq k) => k -> AssocMap k v -> Maybe v
lookup key (AssocMap xs) = lookup' key xs
  where
    lookup' _ [] = Nothing
    lookup' k ((k', value) : xs')
      | k == k' = Just value
      | otherwise = lookup' k xs'

findWithDefault :: (Eq k) => v -> k -> AssocMap k v -> v
findWithDefault defaultVal key m = case Data.AssocMap.lookup key m of
  Nothing -> defaultVal
  Just val -> val

member :: (Eq k) => k -> AssocMap k v -> Bool
member item (AssocMap xs) = member' item xs
  where
    member' :: (Eq k) => k -> [(k, v)] -> Bool
    member' x m = case Prelude.lookup x m of
      Just _ -> True
      Nothing -> False

alter :: (Eq k) => (Maybe v -> Maybe v) -> k -> AssocMap k v -> AssocMap k v
alter func k (AssocMap m) = AssocMap (alter' func k m)
  where
    alter' :: (Eq k) => (Maybe v -> Maybe v) -> k -> [(k, v)] -> [(k, v)]
    alter' f key [] = maybe [] (\value -> [(key, value)]) (f Nothing)
    alter' f key ((key', value') : xs)
      | key == key' = maybe xs (\value -> (key, value) : xs) (f (Just value'))
      | otherwise = (key', value') : alter' f key xs