
substring :: Int -> Int -> [b] -> [b]
substring from to array = take (to - from + 1) (drop from array)


replace position newVal list = take position list ++ newVal : drop (position + 1) list

--Tratamento de entrada
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
caracterInvalid = ['>',' ', '(', ')','|','~','&','-']
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

-- trocar valor das variavel de uma dada formula
changeVariable :: String -> String -> Int->String ->  (String)
changeVariable variavel valores variavelAtual expressao  =
    if variavelAtual == length variavel then expressao
    else  
    changeVariable variavel valores (variavelAtual + 1) (changeVariableAx (variavel !! variavelAtual) (valores !! variavelAtual) expressao 0)

changeVariableAx :: Char -> Char -> String -> Int -> String
changeVariableAx variavel valor express idice =
    if idice == (length express) then express
    else if express !! idice == variavel then 
        changeVariableAx variavel valor (replace idice valor express) (idice+1)

    else changeVariableAx variavel valor express (idice+1)
    
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------


----Encontrando Subformula
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------

find_subformulaExpress :: String -> Int -> ([String], Int)
find_subformulaExpress exp id =
  -- fica pulando para o proximo id até encontrar um caracter valido
  if (exp !! id) == ' ' || (exp !! id) == '>' || (exp !! id) == '~' 
    then find_subformulaExpress exp (id + 1)
    else -- encontra subformulas que estão dentro de parenteses

      if (exp !! id) == '('
        then do
          let var = (find_subformula_aux exp (id + 1) 1)
          (expressControllerExpress (substring id var exp), var)
       
        else 
          if (exp !! id) == '~' then 
            if (exp !! (id+1)) == '(' then  do 
              let var = (find_subformula_aux exp (id + 2) 1) 
              (expressControllerExpress (substring id var exp), var)
            else ([[exp !! id ,exp !! (id +1)]], (id+1))
            
          else ([[exp !! id]], (id+1))



--encontra as subformulas
operationControllerExpress :: String -> [String]
operationControllerExpress express = case (express !! 0) of
  -- 
  ('|') -> do
    let (first, id) = find_subformulaExpress express 1
    let (second, id2) = find_subformulaExpress express (id + 1)
    
    first ++ second ++ [filter (\xs -> (xs /= ' ' )) (substring 1 id express)] ++ [filter (\xs -> (xs /= ' ')) (substring (id + 1) id2 express)]
  ('&') -> do
    let (first, id) = find_subformulaExpress express 1
    let (second, id2) = find_subformulaExpress express (id + 1)
    first ++ second ++ [filter (\xs -> (xs /= ' ')) (substring 1 id express)] ++ [filter (\xs -> (xs /= ' ')) (substring (id+1) id2 express)]
  ('-') -> do
    let (first, id) = find_subformulaExpress express 1
    let (second, id2) = find_subformulaExpress express (id + 1)
    first ++ second ++ [filter (\xs -> (xs /= ' ')) (substring 2 id express)] ++ [filter (\xs -> (xs /= ' ')) (substring (id+1) id2 express)]
  ('(') -> expressControllerExpress express
  ('~') -> do  
    let (first, id) = find_subformulaExpress express 1
    first ++ [filter (\xs -> (xs /= ' ')) (substring 0 id express)]

    
  (_) -> [express]
-- verifica a integridade da formula ou subformula e retorna erro se não estiver de acordo com a sintaxe

expressControllerExpress :: String -> [String]
expressControllerExpress attr =
  if length attr < 3
    then [attr]
    else if (attr !! 0) == '(' && (attr !! (length attr -1)) == ')' then operationControllerExpress (substring 1 (length attr -2) attr) else error "Formato da equação não condiz com o esperado(1)"

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------




----Execução Tabela Verdade
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--codigo para transformar um numero em binario
toBinary :: (Num a2, Integral a1) => a1 -> [a2]
toBinary 0 = [0]
toBinary n = reverse (binaryHelper n)
binaryHelper 0 = []
binaryHelper n
  | (mod) n 2 == 1 = 1 : binaryHelper ((div) n 2)
  | (mod) n 2 == 0 = 0 : binaryHelper ((div) n 2)

-- transforma o length do array para o valor informado
equalizeLen :: [Int] -> Int -> [Int]
equalizeLen array len = if ((/=) (length array) len) then equalizeLen ([0] ++ array) len else array

-- transforma o booleano em V e F
turnTrueFalse :: [Int] -> Int -> String -> String
turnTrueFalse array id new= if length array == id then new else if array !! id == 0 
    then turnTrueFalse array (id+1) (new ++ "V") else  turnTrueFalse array (id+1) (new ++ "F")

--codigo execução para tabela verdade (linha)
tabelaVerdade :: [String] -> String-> Int -> Int -> IO()
tabelaVerdade formulas variaveis currentId maxId = do
  if currentId <= maxId then do
    let variavelValor = turnTrueFalse (equalizeLen (toBinary(currentId)) (length variaveis)) 0 ""

    tabelaVerdadeAx formulas variaveis variavelValor 0
    putStr("\n")
    tabelaVerdade formulas variaveis (currentId+1) maxId

  else putStr("\n")

--codigo auxiliar para execução do loop da tabela verdade(por coluna)
tabelaVerdadeAx :: [String] -> String -> String -> Int-> IO()
tabelaVerdadeAx formulas variaveis variavelValor currentId =
  if currentId < length formulas 
    then do
          if currentId /= 0 then putStr("|") else putStr(" ")
          putStr("\t")
          let form = formulas !! currentId

          let processForm = changeVariable variaveis variavelValor  0 form
          

          putStr (expressController (processForm))
          putStr("\t")
          tabelaVerdadeAx formulas variaveis variavelValor (currentId+1)
  else putStr("")

    

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
expressController attr = 
  if attr !! 0 == '~' then if expressController(substring 1 (length attr -1) attr) == "V" then "F"
  else "V"
  else if length attr < 3 then do
        let (exp, id) = find_subformula attr 0
        exp
    else if (attr !! 0) == '(' && (attr !! (length attr -1)) == ')' then operationController(substring 1 (length attr -2) attr) else error "Formato da equação não condiz com o esperado(1)"

--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------
--codigo necessario para printar subformulas
printCabecalho subform id = do
  if id < (length subform) then do
    if id == 0 then putStr("") else putStr("|")
    putStr("\t")
    putStr(subform !! id)
    putStr ("\t")
    printCabecalho subform (id+1)

  else putStr("\n\n")

-- codigo para indentificar se a formlua é tautologia, insatisfatível ou satisfatível
analiseFormula :: String -> String -> Int -> Int -> Int-> IO()
analiseFormula formula variavel currentId maxId trueContador= do
  if currentId <= maxId
    then do
      let variavelValor = turnTrueFalse (equalizeLen (toBinary(currentId)) (length variavel)) 0 ""

      let processForm = changeVariable variavel variavelValor 0 formula

      if (expressController (processForm)) == "V" then analiseFormula formula variavel (currentId+1) maxId (trueContador+1)
      else analiseFormula formula variavel (currentId+1) maxId trueContador
      
      
  else if trueContador == (maxId+1) then putStrLn("\nEssa formula é uma tautologia") 
  else if trueContador == 0 then putStrLn("\nEssa formula é insatisfatível")
  else putStrLn("\nEssa formula é satisfatível")

main = do
    putStrLn("Digite a formula")
    input <- getLine
    let variable = removeDuplicate (getAllVariable input 0)
    let currentNumber = 0
    let maxNumber = 2 ^ (length variable) -1
    let subform = removeDuplicate((expressControllerExpress input)++ [input])
    printCabecalho subform 0
    tabelaVerdade subform variable currentNumber maxNumber
    analiseFormula input variable 0 maxNumber 0