

split :: String -> [String]
split attr = words attr


-- transformar inteiro em binario
toBin 0 = [0]
toBin n = reverse (helper n)

-- função auxiliar da função toBin
helper 0 = []
helper n
  | (mod) n  2 == 1 = 1 : helper ((div)n  2)
  | (mod) n 2 == 0 = 0 : helper ((div) n  2)


-- Função que adiciona zero no final da lista até atingir um resultado sastifatorio
equalizeLen :: [Integer]  -> Int ->  [Integer]
equalizeLen array len= if ((/=)(length array) len) then equalizeLen ([0]++array) len  else array 

isAtomic :: String -> Bool
isAtomic attr = if (/= ) (length attr)  1 then False else True


binToBool :: [Integer] -> Int -> Bool
binToBool list pos = if (/=) (list !! pos) 1 then True else False





main = do
    -- print((split "oi gente") !! 0)
    putStrLn("Escreva as variaveis da tabela verdade")
    input <- getLine
    let variavel = split input
    putStrLn("escreva a expressão da tabela verdade")
    fuction <- getLine

    print(toBin 8)
    print(length (toBin 8))
    print(equalizeLen (toBin 8) 10 )
