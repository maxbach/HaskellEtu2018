import           Data.Char

isPalindrome :: String -> Bool
isPalindrome [] = True
isPalindrome [_] = True
isPalindrome (fst:mid) = (toLower fst) == (toLower (last mid)) && isPalindrome (init mid)
