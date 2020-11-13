removeDuplicate :: Eq a => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (i : lista)
  | i `elem` lista = removeDuplicate lista
  | otherwise = i : removeDuplicate lista


main = do
    var <- getLine;
    print(removeDuplicate var)