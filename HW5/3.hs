import Data.List
import Data.Char

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr (fun a) b
fun begin ch = if (begin > ch) then Nothing else Just(ch, chr(ord ch - 1))
