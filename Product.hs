module Product where

data Product xs where
  And  :: x -> Product xs -> Product (x : xs)
  None :: Product '[]

class Contains x xs where
  getElem :: Product xs -> x
  modElem :: (x -> x) -> Product xs -> Product xs

instance {-# OVERLAPS #-} Contains x (x : xs) where
  getElem   (And x _) = x
  modElem f (And x xs) = And (f x) xs

instance Contains x xs => Contains x (y : xs) where
  getElem   (And _ xs) = getElem xs
  modElem f (And x xs) = And x (modElem f xs)


