import Text.Printf

data OrderLine
  = ProductLine { productCode :: String, productQuantity :: Int, price :: Float, lineTotal :: Float }
  | TotalLine { totalQuantity :: Int, orderTotal :: Float }


instance Show OrderLine where
  show (ProductLine plCode plQuantity plPrice plTotal) =
    printf "%-10s %5i @%4g each %6g" plCode plQuantity plPrice plTotal
  show (TotalLine tlQuantity tlTotal) =
    printf "%-10s %5i            %6g" "TOTAL" tlQuantity tlTotal


sampleLines :: [OrderLine]
sampleLines =
  [ ProductLine { productCode = "AAA", productQuantity = 2, price = 2.99, lineTotal = 5.98 },
    ProductLine { productCode = "BBB", productQuantity = 1, price = 1.99, lineTotal = 1.99 },
    ProductLine { productCode = "CCC", productQuantity = 3, price = 3.99, lineTotal = 11.97 }
  ]


addLine :: OrderLine -> OrderLine -> OrderLine
addLine (ProductLine "" _ _ _) line = line
addLine line (ProductLine "" _ _ _) = line
addLine (ProductLine _ plQuantity _ plTotal) (TotalLine tlQuantity tlTotal) =
  TotalLine {
    totalQuantity = plQuantity + tlQuantity,
    orderTotal = plTotal + tlTotal
  }
addLine (TotalLine tlQuantity tlTotal) (ProductLine _ plQuantity _ plTotal) =
  TotalLine {
    totalQuantity = plQuantity + tlQuantity,
    orderTotal = plTotal + tlTotal
  }
addLine (TotalLine tlQuantity1 tlTotal1) (TotalLine tlQuantity2 tlTotal2) =
  TotalLine {
    totalQuantity = tlQuantity1 + tlQuantity2,
    orderTotal = tlTotal1 + tlTotal2
  }
addLine line1 line2 =
  TotalLine {
    totalQuantity = productQuantity line1 + productQuantity line2,
    orderTotal = lineTotal line1 + lineTotal line2
  }


emptyLine :: OrderLine
emptyLine =
  ProductLine {
    productCode = "",
    productQuantity = 0,
    price = 0,
    lineTotal = 0
  }


totalLine :: [OrderLine] -> OrderLine
totalLine lines = foldl addLine emptyLine lines


main = do
  mapM_ print sampleLines
  putStrLn "----------------------------------"
  print subtotal
  putStrLn "----------------------------------"
  mapM_ print sampleLines
  putStrLn "----------------------------------"
  print bigTotal
  where
    subtotal = totalLine sampleLines
    bigTotal = totalLine $ sampleLines ++ [subtotal]



