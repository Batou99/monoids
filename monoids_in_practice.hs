import Text.Printf
import Data.Monoid

data OrderLine = OrderLine { productCode :: String, productQuantity :: Int, price :: Float, lineTotal :: Float }
data TotalLine = TotalLine { totalQuantity :: Int, orderTotal :: Float }


instance Show OrderLine where
  show (OrderLine plCode plQuantity plPrice plTotal) =
    printf "%-10s %5i @%4g each %6g" plCode plQuantity plPrice plTotal


instance Show TotalLine where
  show (TotalLine tlQuantity tlTotal) =
    printf "%-10s %5i            %6g" "TOTAL" tlQuantity tlTotal


sampleLines :: [OrderLine]
sampleLines =
  [ OrderLine { productCode = "AAA", productQuantity = 2, price = 2.99, lineTotal = 5.98 },
    OrderLine { productCode = "BBB", productQuantity = 1, price = 1.99, lineTotal = 1.99 },
    OrderLine { productCode = "CCC", productQuantity = 3, price = 3.99, lineTotal = 11.97 }
  ]


toTotalLine :: OrderLine -> TotalLine
toTotalLine line =
  TotalLine { totalQuantity = productQuantity line, orderTotal = lineTotal line }


instance Monoid TotalLine where
  mappend line1 line2 =
    TotalLine {
      totalQuantity = totalQuantity line1 + totalQuantity line2,
      orderTotal = orderTotal line1 + orderTotal line2
    }
  mempty = TotalLine 0 0


main = do
  mapM_ print sampleLines
  putStrLn "----------------------------------"
  print subtotal
  putStrLn "----------------------------------"
  mapM_ print sampleLines
  putStrLn "----------------------------------"
  print bigTotal
  where
    samplesAsTotals = map toTotalLine sampleLines
    subtotal = mconcat samplesAsTotals
    bigTotal = mconcat $ samplesAsTotals ++ [subtotal]
