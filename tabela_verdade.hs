slice :: Int -> Int -> [b] -> [b]
slice from to array = take (to - from + 1) (drop from array)


findChar :: String -> Char -> Int-> Bool
findChar exp char id = if (exp !! id) == char then True else if (id + 1) == (length exp) then False else findChar exp char (id + 1)


orController :: String -> [String]
orController exp = if (findChar exp '(' 0) then [""] else ["testando"]


operationController :: String -> [String]
operationController express = case (express !! 0) of
    ('|') -> [""]
    (_) -> error "Formato da equação não condiz com o esperado(2)"


expressController :: String -> [String]
expressController attr = if (attr !! 0) == '(' && (attr !! (length attr -1)) == ')' then operationController(slice 1 (length attr -2) attr) else error "Formato da equação não condiz com o esperado(1)"

main = do
    input <- getLine
    
    let response = expressController(input)
    print(response)