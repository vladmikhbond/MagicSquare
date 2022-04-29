-- ("vgcm","")
-- ("vc","gm")
-- ("vcm","g")
-- ("v","gcm")
-- ("vgm","c")
-- ("g","vcm")
-- ("gm", "vc")
-- ("", "vgcm")

import Data.List ( (\\), sort)

-- состояние - упоряд. множ. объектов, включая лодку, на левом берегу (множ на правом = разность)
type State = [Char]  
type Hist  = [State] 

boat : goods = "_cgv"
invalidStates = map sort ["gv", "cg", "cgv", "_c", "_v", "_"]

-- переходы: если лодка на лев берегу, удалить пустую лодку или лодку с любым объектом
--           иначе добавить пустую лодку или лодку с любым объектом с правого берега
nextStates :: State -> [State]
nextStates state = let
   thisBank = state \\ [boat]
   otherBank = goods \\ state
   states = if boat `elem` state
    then thisBank : [ thisBank \\ [x] | x <- thisBank]
    else (boat : state) : [boat : x : state | x <- otherBank]
 in 
   map sort states

isValid :: State -> Bool
isValid state = state `notElem` invalidStates

variants :: Hist -> [Hist]
variants hist = do 
   let nexts = filter isValid (nextStates $ head hist) 
   next <- filter (`notElem` hist) nexts 
   if null next 
     then return $ reverse hist   
     else variants (next : hist)
      
solve = variants [boat : "cgv"]