import Data.Map as Map
import Data.Array
withdraw account amount bank = 
    case Map.lookup account bank of
        Nothing -> bank
        Just sum -> Map.insert account (sum - amount) bank

myArray :: Array Bool String 
myArray = array (True, False) [(True, "t"), (False , "f")]

