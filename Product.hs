{-|
  The heterogeneous list.
-}

module Product where

-- | `Product xs` contains elements of each of the types from the `xs` list.
data Product xs where
  And  :: x -> Product xs -> Product (x : xs)
  None :: Product '[]

-- | Find/modify the element with a given type.
--
--   If you want to have same-types, use newtype wrappers.
--
class Contains x xs where
  getElem :: Product xs -> x
  modElem :: (x -> x) -> Product xs -> Product xs

instance {-# OVERLAPS #-} Contains x (x : xs) where
  getElem   (And x _) = x
  modElem f (And x xs) = And (f x) xs

instance Contains x xs => Contains x (y : xs) where
  getElem   (And _ xs) = getElem xs
  modElem f (And x xs) = And x (modElem f xs)

-- | Add a name to the type.
--
newtype (s :: String) := t = Tag { unTag :: t }

-- | Retrieve a type associated with the given name.
--
getTag :: forall s t xs. Contains (s := t) xs => Product xs -> t
getTag = unTag . getElem @(s := t)

-- | Modify a type associated with the given name.
--
modTag
  :: forall s t xs
  .  Contains (s := t) xs
  => (t -> t)
  -> Product xs -> Product xs
modTag f = modElem @(s := t) (Tag . f . unTag)