module Monoids where

class Group g where
  identity :: g
  add :: g -> g -> g
  inverse :: g -> g

class Monoid m where
  mempty :: m
  mappend :: m -> m -> m
  mconcat :: [m] -> m
  mconcat = foldr mappend mempty


newtype Sum a = Sum a
instance Num a => Monoid (Sum a) where
  mempty = Sum 0
  mappend (Sum x) (Sum y) = Sum $ x + y

sum :: Num a => [a] -> a
sum nums = s
  where Sum s = mconcat $ map Sum nums


newtype Product a = Product a
instance Num a => Monoid (Product a) where
  mempty = Product 1
  mappend (Product x) (Product y) = Product $ x * y
  
product :: Num a => [a] -> a
product nums = p
  where Product p = mconcat $ map Product nums


newtype All = All Bool
instance Monoid All where
  mempty = All True
  mappend (All x) (All y) = All $ x && y

all :: [Bool] -> Bool
all bools = b
  where All b = mconcat $ map All bools


newtype Any = Any Bool
instance Monoid Any where
  mempty = Any False
  mappend (Any x) (Any y) = Any $ x || y

any :: [Bool] -> Bool
any bools = b
  where Any b = mconcat $ map Any bools


type OutputTypeId = Int
type Quantity = Fractional
data Output = Output OutputTypeId Quantity


instance Monoid [Output] where
  mempty = []
  mappend output1 output1 =

  (<>) :: Output -> Output -> Output
  (<>) output1 output2 =


