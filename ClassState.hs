module ClassState
where

import Data.Map (Map)
import qualified Data.Map as Map

-- Utilizat pentru a obține informații despre Variabile sau Funcții
data InstrType = Var | Func deriving (Show, Eq)

-- TODO - Trebuie definit ClassState

-- clasa mea va fi o pereche (Map pentru variabile, Map pentru functii)
type ClassState = (Map String String, Map String [String])

-- o clasa goala = pereche de map-uri goale
initEmptyClass :: ClassState
initEmptyClass = (Map.empty, Map.empty)

-- inserarea se face in primul map pentru variabile 
-- si in al doilea pentru functii
insertIntoClass :: ClassState -> InstrType -> [String] -> ClassState
insertIntoClass (varMap, funcMap) instr (sym:(vfType:list))
    | instr == Var = (Map.insert sym vfType varMap, funcMap)
    | otherwise = (varMap, Map.insert sym (vfType:list) funcMap)


-- se face o interogare fie pe primul Map (variabile), fie pe al doilea (functii)
getValues :: ClassState -> InstrType -> [[String]]
getValues (varMap, funcMap) instr
    | instr == Var = map (\(x, y) -> [x, y]) (Map.toList varMap)
    | otherwise = map (\(x, l) -> x:l) (Map.toList funcMap)