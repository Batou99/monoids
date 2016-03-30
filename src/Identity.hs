module Identity where


data PositiveNumberOrIdentity
  = Positive Int
  | Zero


instance Monoid PositiveNumberOrIdentity where
  mempty = Zero
  mappend Zero i = i
  mappend i Zero = i
  mappend (Positive i1) (Positive i2) = Positive (i1 + i2)


main :: IO()
main = undefined

