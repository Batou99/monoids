import Text.Printf

data OrderLine = OrderLine {
  productCode :: String,
  quantity :: Int,
  total :: Float
}


instance Show OrderLine where
  show orderLine =
    printf "%-10s %5i %6g" olCode olQuantity olTotal
    where
      OrderLine olCode olQuantity olTotal  = orderLine


calculateOrderTotal :: [OrderLine] -> Float
calculateOrderTotal lines =
  foldl (+) 0 $ map total lines


sampleLines :: [OrderLine]
sampleLines =
  [ OrderLine { productCode = "AAA", quantity = 2, total = 2.99 },
    OrderLine { productCode = "BBB", quantity = 1, total = 1.99 },
    OrderLine { productCode = "CCC", quantity = 3, total = 3.99 }
  ]


addLine :: OrderLine -> OrderLine -> OrderLine
addLine line1 line2 =
  OrderLine {
    productCode = "TOTAL",
    quantity = quantity line1 + quantity line2,
    total = total line1 + total line2
  }


main = do
  mapM_ print sampleLines
  print $ calculateOrderTotal sampleLines



