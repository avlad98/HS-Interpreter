module Parser
where

import Util
import Data.Maybe
import InferenceDataType
import Data.String
import Data.Char
import Data.List
import Data.Map (Map)
import qualified Data.Map as Map

-- Definire Program
            --      Vars       ClassName   Funcs   SuperClass
type Program = ([[String]], Map String ([[String]], String))
type Instruction = [String]


-- Program-ul gol este (lista goala pt variabile, Map cu clasa "Global")
initEmptyProgram :: Program
initEmptyProgram = ([], Map.insert "Global" ([], "Global") Map.empty)

-- Aceasta functie decat intoarce primul element din pereche, adica
-- lista cu variabile 
getVars :: Program -> [[String]]
getVars program = fst program

-- Aceasta functie interogheaza map-ul pentru lista de chei, adica
-- lista cu toate numele de clase
getClasses :: Program -> [String]
getClasses program = Map.keys (snd program)

-- Aceasta functie cauta clasa dorita in Map si intoarce al doilea element
-- din pereche, adica numele superclasei
getParentClass :: String -> Program -> String
getParentClass className program = snd (fromJust (Map.lookup className (snd program)))

-- Aceasta functie intoarce lista goala daca numele clasei nu exista in Map
-- Pentru clasele care exista se cauta in map, se ia valoarea din Just si 
-- se intoarce lista de functii stocata la clasa corespunzatoare
getFuncsForClass :: String -> Program -> [[String]]
getFuncsForClass className program
    | (Map.member className (snd program) == False) = []
    | otherwise = fst (fromJust (Map.lookup className (snd program)))


-- Functia de parsare primeste input-ul din fisier si face o serie de operatii
-- pe acesta pentru a-l aduce la o forma cat mai simpla
parse :: String -> [Instruction]
parse text = filter (not . null) (sndPair ++ fstPair)
    where 
        -- replaceAll inlocuieste toate caracterele ":(),=." cu spatiu pentru parsare mai usoara
        replaceAll = map (\c -> if c==':' || c=='(' || c==')' || c==',' || c=='=' || c=='.' then ' '; else c)
        linesText = lines text  -- tranform inputul in lista de linii
        pair = partition (isPrefixOf "infer ") linesText -- impart liniile cu infer de celelalte pentru parsare diferita
        sndPair = map words (map replaceAll (snd pair)) -- liniile care nu sunt pentru infer
        fstPair = map words (fst pair) -- liniile cu infer

-- Functia de interpretare executa anumite instructini in functie de keyword-ul instructiunii (primul
-- cuvant din lista)
interpret :: Instruction -> Program -> Program
interpret list program
    -- se insereaza o clasa noua care extinde alta clasa (doar daca superclasa exista)
    | (key == "class") && classCond = (fst program, Map.insert name ([], superClass) (snd program))
    
    -- se insereaza o clasa simpla sau clasa care vrea sa extinda alta clasa, dar nu exista
    | (key == "class") = (fst program, Map.insert name ([], "Global") (snd program))
    
    -- se insereaza o noua variabila doar daca tipul ei exista in lista de clase
    | (key == "newvar") && varCond = (newVarList, snd program)
    
    -- se insereaza o functie doar daca verificarile necesare sunt indeplinite cu succes
    | (funcCond == True) = newProgram
    
    -- se efectueaza infer pe expresia primita
    | (key == "infer") = if (inferCond) then newInferProg else program
    
    -- daca niciuna din cele de mai sus inseamna ca nu sunt indeplinite conditiile necesare
    -- pentru instructiunea curenta, deci se va ignora
    | otherwise = program
        where 
            key = (!!) list 0
            -- for classes
            name = (!!) list 1
            superClass = (!!) list 3
            classCond = (elem "extends" list) && (Map.member superClass (snd program))
            
            -- for variables
            varName = (!!) list 1
            varClass = (!!) list 2
            varCond = Map.member varClass (snd program)
            newVarList = (varName:varClass:[]):(fst program) -- se creeaza noua lista de variabile


            -- for functions
            funcRet = (!!) list 0
            funcParent = (!!) list 1
            funcName = (!!) list 2
            funcParams = drop 3 list
            -- funcCond verifica daca superclasa, tipul de return si toti parametrii exista in Map
            funcCond = (Map.member funcParent (snd program)) && (Map.member funcRet (snd program)) && testParams
            -- testParams este True daca parametrii primiti se regasesc in clasele existente din Map
            testParams = (intersect funcParams (Map.keys (snd program))) == funcParams
            -- newProgram = programul actualizat cu noua functie introdusa in clasa respectiva
            newProgram = (fst program, Map.insert funcParent newFuncPair (snd program))
            oldFuncPair = fromJust (Map.lookup funcParent (snd program))
            newFuncPair = (theFunc:(fst oldFuncPair), snd oldFuncPair)
            theFunc = funcName:funcRet:funcParams

            -- for infer
            -- pentru usurinta inlocuiesc '=' cu space pentru a putea sparge string-ul in cuvinte
            replaceSpc = map (\c -> if c=='='then ' '; else c)
            -- initial = string-ul neparsat (fara cuvantul "infer")
            initial = unwords (drop 1 list)
            -- inferVarName = numele variabilei din expresie
            inferVarName = head (words (replaceSpc initial))
            -- expr = ceea ce este dupa '=' in input
            expr = unwords (tail (words (replaceSpc initial)))
            -- inferRes = rezultatul final
            inferRes = evalExpr expr program
            -- inferCont = conditia de verificare ca s-a efectuat cu succes inferarea
            inferCond = isJust inferRes
            -- newInferProg = programul actualizat cu variabila introdusa
            newInferProg = interpret ["newvar",inferVarName,(fromJust inferRes)] program

            -- evalExpr :: String -> Program -> Maybe String
            evalExpr expr program = evalRes
                where
                    -- infVar = numele variabilei din expresie
                    infVar = takeWhile (/= '.') expr
                    -- theFunc = restul string-ului fara variabila
                    theFunc = tail (dropWhile (/= '.') expr)
                    -- infFunc = numele functiei din restul expresiei
                    infFunc = takeWhile (/= '(') theFunc
                    -- infParams = lista de parametrii neparsata -> "expr1, expr2.."
                    infParams = dropWhile (/= '(') theFunc
                    -- argList = lista de parametrii parsata -> ["expr1", "expr2"...]
                    argList = map (filter (/=' ')) (splitByComma (firstLast infParams))
                    -- exprList = lista de parametrii transformata in lista de expresii -> [expr1, expr2...]
                    exprList = getExpr argList
                    -- compExprList = lista de argumente calculata cu infer
                    compExprList = map ((flip infer) program) exprList
                    -- evalRes = rezultatul final pentru expresia principala
                    evalRes = infer (FCall infVar infFunc exprList) program

-- getExpr prelucreaza lista de expresii sub forma de String si intoarce lista de expresii
-- getExpr :: [String] -> [Expr]
getExpr [] = []
getExpr (x:xs) = if (notElem '(' x) then (Va x):(getExpr xs) else funcExpr:(getExpr xs)
    where
        fVar = takeWhile (/= '.') x
        fFunc = takeWhile (/= '(') (tail (dropWhile (/= '.') x))
        preArgs = dropWhile (/= '(') (tail (dropWhile (/= '.') x))
        fArgList = map (filter (/=' ')) (splitByComma (firstLast preArgs))
        funcExpr = FCall fVar fFunc (getExpr fArgList)


-- o implementare particulara a functiei split din Data.List.Split (care se pare ca nu pot
-- sa o folosesc pe checker pentru ca nu e instalat "cabal install Split")
-- sparg lista de argumente in expresii doar cand numarul parantezelor deschide e egal cu cele
-- inchise
splitByComma :: String -> [String]
splitByComma string = splitAux string [] [] 0 0
    where
        splitAux [] crt acc opened closed = acc ++ [crt]
        splitAux (',':s) crt acc opened closed = if (opened == closed) then (splitAux s [] (acc++[crt]) 0 0) else (splitAux s (crt++[',']) acc opened closed)
        splitAux ('(':s) crt acc opened closed = splitAux s (crt++['(']) acc (opened + 1) closed
        splitAux (')':s) crt acc opened closed = splitAux s (crt++[')']) acc opened (closed + 1)
        splitAux (c:s) crt acc opened closed = splitAux s (crt++[c]) acc opened closed

-- firstLast sterge '(' si ')' din String-ul cu argumente (primul si ultimul char)
firstLast::[a]->[a]
firstLast [] = []
firstLast [x] = []
firstLast xs = tail (init xs)

-- intoarce tipul variabilei sau Nothing daca aceasta nu exista
getVarType :: String -> [[String]] -> Maybe String
getVarType _ [] = Nothing
getVarType name (l:list) = if (elem name l) then (Just ((!!) l 1)) else (getVarType name list)

-- intoarce o lista cu toate functiile incepand din subclasa pana la global
-- adica toate functiile din lantul de mostenire
getAllFunctionsFrom :: String -> Program -> [[String]]
getAllFunctionsFrom className program = if (className /= "Global") then (getFuncsForClass className program) ++ (getAllFunctionsFrom superClass program) else []
    where
        superClass = snd (fromJust (Map.lookup className (snd program)))

-- infer pentru subpunctul c
-- pentru variabile ma folosesc de getVarType
-- pentru functii verific conditiile specificate in enunt si apoi calculez rezultatul recursiv
infer :: Expr -> Program -> Maybe String
infer (Va varName) program = getVarType varName (getVars program)
infer (FCall varName funcName expr) program = if inferFuncCond then inferFuncRes else Nothing
    where
        varClass = infer (Va varName) program
        startingClass = fromJust varClass
        inferFuncCond = (isJust varClass) && (matchingFuncs /= []) && (notElem Nothing paramListMaybe) && (isJust inferFuncRes) && (sndMatchingFuncs /= [])
        inferFuncRes = Just (head (tail (head sndMatchingFuncs)))
        matchingFuncs = funcExists funcName functionList -- lista de functii care au acelasi nume cu cel cautat
        sndMatchingFuncs = filter (isSubsequenceOf paramList) matchingFuncs --lista finala de functii (filtrata mai bine)
        functionList = filter (not . null) (getAllFunctionsFrom startingClass program)
        paramListMaybe = testParams expr -- lista de Maybe
        paramList = map fromJust paramListMaybe

        funcExists _ [] = []
        funcExists name (l:list) = if (elem name l) then filter (not . null) (l:(funcExists name list)) else funcExists name list
        
        testParams [] = []
        testParams (e:exprList) = (infer e program):(testParams exprList)
