
substring :: Int -> Int -> [b] -> [b]
substring from to array = take (to - from + 1) (drop from array)

--Tratamento de entrada
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
caracterInvalid = ['>',' ', '(', ')','|','~','&']
-- pegar os caracteres validos para serem variavel
getAllVariable :: String -> Int-> String
getAllVariable string id =  
    if id == length string then "" else
    if string !! id `elem` caracterInvalid then getAllVariable string (id+1)
    else [(string !! id)] ++ getAllVariable string (id+1)
-- remover duplicatas na lista
removeDuplicate :: Eq a => [a] -> [a]
removeDuplicate [] = []
removeDuplicate (i : lista)
  | i `elem` lista = removeDuplicate lista
  | otherwise = i : removeDuplicate lista
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------




----Execução Tabela Verdade
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
toBinary 0 = [0]
toBinary n = reverse (binaryHelper n)
binaryHelper 0 = []
binaryHelper n
  | (mod) n 2 == 1 = 1 : binaryHelper ((div) n 2)
  | (mod) n 2 == 0 = 0 : binaryHelper ((div) n 2)

equalizeLen :: [Integer] -> Int -> [Integer]
equalizeLen array len = if ((/=) (length array) len) then equalizeLen ([0] ++ array) len else array





--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------


---Compilar uma expressão
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
-- função responsavel para separa as subformulas do resto da formula
find_subformula :: String -> Int -> (String, Int)
find_subformula exp id = 
    -- fica pulando para o proximo id até encontrar um caracter valido
    if (exp !! id) == ' ' || (exp !! id) == '>' then find_subformula exp (id + 1)
    -- encontra subformulas que estão dentro de parenteses
    else if (exp !! id) == '(' then do
        let var = (find_subformula_aux exp (id + 1) 1)
        (substring id var exp, var)
    -- no caso de ser uma negação de uma subformula atomica ou a propria subformula atomica
    else if exp !! id == 'V' then ("V", id)
    else if exp !! id == 'F' then ("F", id)
    else if exp !! id == '~' then do
        let (sentece, c) = find_subformula exp (id + 1)
        if sentece == "V" then ("F", c) else ("V", c)
    else error "Formato da função não condiz com o esperado"
-- auxilia a função acima a encontra a o fechamento do parentese
find_subformula_aux :: String -> Int -> Int -> Int
find_subformula_aux exp current parenteses = if parenteses == 0 then (current - 1) 
    else if exp !! current == '('  then find_subformula_aux exp (current + 1) (parenteses + 1) 
    else if exp !! current == ')' then find_subformula_aux exp (current + 1) (parenteses - 1)
    else find_subformula_aux exp (current + 1) parenteses

-- verifica o operador que será utilizado para resolver a formula
operationController :: String -> String
operationController express = case (express !! 0) of
    -- codigo referente ao or
    ('|') -> do 
        let (first, id) = find_subformula express 1
        let (second, id2) = find_subformula express (id + 1)
        if (expressController first) == "F" && (expressController second) == "F" then "F"
        else "V"
    -- codigo refenrete ao and
    ('&') -> do
        let (first, id) = find_subformula express 1
        let (second, id2) = find_subformula express (id + 1)
        if (expressController first) == "V" && (expressController second) == "V" then "V"
        else "F"
    -- codigo referente ao se
    ('-') -> do
        let (first, id) = find_subformula express 1
        let (second, id2) = find_subformula express (id + 1)
        if (expressController first) == "V" && (expressController second) == "F" then "F"
        else "V"
    -- codigo para permitir tratar parentes desnecessarios
    ('(') -> expressController express
    -- codigo referente a negação
    ('~') -> do
        if expressController(substring 1 (length express -1 ) express) == "V" then "F"
        else "V"
    -- codigo referente a expressao apolar
    ('V') -> "V"
    ('F') -> "F"
    -- erro caso encontre um caracter diferente do esperado
    (_) -> error "Formato da equação não condiz com o esperado(2)"
-- verifica a integridade da formula ou subformula e retorna erro se não estiver de acordo com a sintaxe
expressController :: String -> String
expressController attr = if length attr < 3 then do
        let (exp, id) = find_subformula attr 0
        exp
    else if (attr !! 0) == '(' && (attr !! (length attr -1)) == ')' then operationController(substring 1 (length attr -2) attr) else error "Formato da equação não condiz com o esperado(1)"

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

main = do
    input <- getLine
    let variable = removeDuplicate (getAllVariable input 0)
    let currentNumber = 0
    let maxNumber = 2 ^ (length variable)
    -- let response= expressController input 
    -- print(response) 