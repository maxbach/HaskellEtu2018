module FP3 where

import Data.List
import Data.Maybe

revRange :: (Char,Char) -> [Char]
revRange (a, b) = unfoldr (fun a) (Just b)

fun :: Char -> Maybe Char -> Maybe (Char, Maybe Char)
fun begin ch = if ((isNothing ch) || (begin > (fromJust ch))) then Nothing
else if ((fromJust ch) == '\NUL') then Just('\NUL', Nothing)
else Just ((fromJust ch), Just(pred (fromJust ch)))
