test :: IO ()
test = do
    let x = maxBound :: Int
    print (x + 1)
