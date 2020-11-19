

split :: String -> [String]
split attr = words attr

replace position newVal list = take position list ++ newVal : drop (position + 1) list

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

solveAtomicExpress :: [String] ->  Bool
solveAtomicExpress express = case (express !! 1) of
    ("->") ->  ifAtomicExpress(express)
    ("&") -> andAtomicExpress(express)
    ("|") -> orAtomicExpress(express)
  

ifAtomicExpress :: [String] -> Bool
ifAtomicExpress express = if ((express !! 0) == "1" && (express !! 2) == "0") then  False else  True

orAtomicExpress :: [String] -> Bool
orAtomicExpress express = if ((express !! 0) == "1" || (express !! 2) == "1") then True else False

andAtomicExpress :: [String] -> Bool
andAtomicExpress express = if ((express !! 0) == "1" && (express !! 2) == "1") then True else False


-- identifyVariable :: String -> Integer -> [Char]
-- identifyVariable express currentId=

main = do
    -- print((split "oi gente") !! 0)
    --------------------------------------
    -- Bloco de Codigo de Teste
    input <- getLine

    print(solveAtomicExpress(split(input)))
    putStr("oi")


    -------------------------------------
    --- Bloco de Codigo funcional
    -- putStrLn("Escreva as variaveis da tabela verdade")
    -- input <- getLine
    -- let variavel = split input
    -- putStrLn("escreva a expressão da tabela verdade")
    -- fuction <- getLine

    -- print(toBin 8)
    -- print(length (toBin 8))
    -- print(equalizeLen (toBin 8) 10 )
