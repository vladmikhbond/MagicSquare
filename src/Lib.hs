-- ("vgcm","")
-- ("vc","gm")
-- ("vcm","g")
-- ("v","gcm")
-- ("vgm","c")
-- ("g","vcm")
-- ("gm", "vc")
-- ("", "vgcm")
--
-- "","mg","g","mgc","c","mvc","vc","vgcm"]
-- ["","mc","c","mvc","vc","vgcm"]
-- ["","mc","c","mcv","cv","v","mvc","vc","vgcm"]
-- состояние - множ персонажей на левом берегу (множ на правом = разность)
-- переходы: если М присутствует, можно удалить одно m или m с любой др буквой
--           иначе добавить одно m или m с любой недостающей буквой  
-- отсечения - наличие подмн "vg", или "cg", при отсутствии m
import Data.List ( (\\), sort)

type State = [Char]  -- v g c m
type Hist  = [State]  -- v g c m

nextStates :: State -> [State]
nextStates st = let
   st' = st \\ ['m']
   right = "vgc" \\ st
 in 
   if 'm' `elem` st
    then st' : [ st' \\ [x] | x <- st']
    else ('m' : st) : ['m' : x : st | x <- right]

isValid :: State -> Bool
isValid state = sort state `notElem` ["gv", "cg", "cgv", "cm", "mv", "m"]

solv :: [Hist] -> [Hist]
solv hists = do 
   hist <- hists   
   let nexts = filter isValid (nextStates $ head hist)  --   ["vgcm"] <- [["vgcm"]]
   next <- filter (`notElem` hist) nexts   --    "vc"    <-   ["vc"]

   if next == "" 
     then [hist]   
     else  solv [next : hist]
      
   