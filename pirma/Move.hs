module Move where

import Parse
import Message
import Data.List
import Data.Maybe
import Data.Either
import Data.Char

getNextCoord :: (String, String) -> Either String (String, String)
getNextCoord (letter, number)
    | head letter >= 'J' && stringToInt(number) >= 10 = Left "Board is full"
    | head letter < 'J' = Right ([chr (ord (head letter) + 1)], number)
    | head letter == 'J' && stringToInt(number) < 10 = Right ("A", show (stringToInt(number) + 1)) 
    | True = Left $ "Something went wrong while getting new coord."
        where
            stringToInt :: String -> Int
            stringToInt ('1':'0':_) = 10
            stringToInt (x: _) = digitToInt x

splitMoves :: Msg -> [(String, String)] -> [(String, String)] -> ([(String, String)], [(String, String)])
splitMoves (Msg ('0':_, '0':_) _ Empty) playerA playerB = (playerA, playerB)
splitMoves (Msg ('0':_, '0':_) _ prev) playerA playerB = splitMoves prev playerB playerA
splitMoves (Msg coords _ Empty) playerA playerB = (coords : playerA, playerB)
splitMoves (Msg coords _ prev) playerA playerB = splitMoves prev playerB (coords:playerA)

getPlayerA :: ([(String, String)], [(String, String)]) -> [(String, String)]
getPlayerA (x, _) = x  

checkIfConflicts :: (String, String) -> Msg -> Bool
checkIfConflicts (x,y) msg = elem (x,y) moves 
    where
        moves = getPlayerA(splitMoves msg [] [])

move :: String -> Either String (Maybe [String])
move msg = 
   case parseMoves msg of
     Left e1 -> Left e1
     Right (msg, rest) ->
        if checkIfFull msg
            then Left "The board is full"
        else 
            case checkIfConflicts ("A", "1") msg of
                True -> Right(Just(findCoord ["A", "1"] msg))
                False -> Right(Just ["A", "1"])

findCoord :: [String] -> Msg -> [String]
findCoord [a,b] msg = 
    case getNextCoord (a,b) of
        Right (c,d) -> 
            case checkIfConflicts (c,d) msg of
                True -> findCoord [c,d] msg
                False -> [c,d]

countas :: [(String, String)] -> Int -> Int  --countina moves
countas [] n = n
countas [x] n = n+1
countas (head:rest) n = countas rest n+1 

checkIfFull :: Msg -> Bool
checkIfFull msg = 
    if countas(getPlayerA(splitMoves msg [] [])) 0 >= 100
        then True
    else False

checkIfMovesValid :: String -> Bool   -- patikrina ar kiekvienas langelis turi max viena ejima
checkIfMovesValid message =
    case parseMoves message of
        Right (msg, rest) ->
            let
                (playerA, playerB) = splitMoves msg [] []
                checkIfExist :: [(String, String)] -> Bool
                checkIfExist [] = False
                checkIfExist (x:coords) = (x `elem` coords) || checkIfExist coords
            in
                not(checkIfExist playerA || checkIfExist playerB)

full = "[\"coord\",[\"B\",\"10\"],\"result\",\"HIT\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"C\",\"9\"],\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"I\",\"4\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"D\",\"9\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"C\",\"5\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"J\",\"8\"],\"prev\",[\"prev\",[\"result\",\"HIT\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"A\",\"7\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"D\",\"5\"],\"prev\",[\"coord\",[\"D\",\"2\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"D\",\"1\"],\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"9\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"F\",\"7\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"A\",\"4\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"J\",\"2\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"G\",\"5\"],\"prev\",[\"coord\",[\"F\",\"5\"],\"prev\",[\"coord\",[\"I\",\"8\"],\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"H\",\"7\"],\"prev\",[\"prev\",[\"coord\",[\"G\",\"2\"],\"result\",\"MISS\",\"prev\",[\"prev\",[\"coord\",[\"D\",\"4\"],\"prev\",[\"coord\",[\"D\",\"7\"],\"prev\",[\"coord\",[\"G\",\"4\"],\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"C\",\"5\"],\"prev\",[\"coord\",[\"A\",\"9\"],\"result\",\"HIT\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"J\",\"1\"],\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"I\",\"4\"],\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"I\",\"10\"],\"prev\",[\"prev\",[\"coord\",[\"H\",\"6\"],\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"C\",\"8\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"A\",\"6\"],\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"H\",\"4\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"B\",\"8\"],\"prev\",[\"coord\",[\"G\",\"1\"],\"result\",\"HIT\",\"prev\",[\"prev\",[\"prev\",[\"result\",\"HIT\",\"prev\",[\"coord\",[\"D\",\"3\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"E\",\"2\"],\"prev\",[\"coord\",[\"F\",\"8\"],\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"I\",\"3\"],\"prev\",[\"coord\",[\"D\",\"8\"],\"prev\",[\"coord\",[\"J\",\"5\"],\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"I\",\"5\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"E\",\"4\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"coord\",[\"E\",\"9\"],\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"A\",\"9\"],\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"J\",\"5\"],\"result\",\"MISS\",\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"E\",\"6\"],\"prev\",[\"prev\",[\"result\",\"HIT\",\"prev\",[\"prev\",[\"coord\",[\"F\",\"8\"],\"prev\",[\"prev\",[\"coord\",[\"J\",\"6\"],\"prev\",[\"prev\",[\"result\",\"HIT\",\"coord\",[\"D\",\"4\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"HIT\",\"prev\",[\"coord\",[\"B\",\"6\"],\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"B\",\"9\"],\"prev\",[\"coord\",[\"E\",\"9\"],\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"G\",\"7\"],\"prev\",[\"result\",\"HIT\",\"coord\",[\"B\",\"2\"],\"prev\",[\"coord\",[\"A\",\"2\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"D\",\"6\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"10\"],\"prev\",[\"coord\",[\"G\",\"6\"],\"prev\",[\"coord\",[\"D\",\"8\"],\"result\",\"MISS\",\"prev\",[\"prev\",[\"coord\",[\"J\",\"7\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"F\",\"2\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"E\",\"10\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"F\",\"4\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"G\",\"3\"],\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"G\",\"1\"],\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"F\",\"7\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"E\",\"7\"],\"prev\",[\"prev\",[\"result\",\"MISS\",\"coord\",[\"A\",\"7\"],\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"coord\",[\"J\",\"10\"],\"result\",\"MISS\",\"prev\",[\"result\",\"HIT\",\"prev\",[\"coord\",[\"J\",\"4\"],\"prev\",[\"prev\",[\"prev\",[\"result\",\"HIT\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"coord\",[\"A\",\"6\"],\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"HIT\",\"coord\",[\"A\",\"1\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"G\",\"3\"],\"prev\",[\"prev\",[\"prev\",[\"result\",\"HIT\",\"coord\",[\"D\",\"5\"],\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"F\",\"6\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"H\",\"10\"],\"prev\",[\"prev\",[\"prev\",[\"result\",\"HIT\",\"coord\",[\"B\",\"1\"],\"prev\",[\"result\",\"HIT\",\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"I\",\"1\"],\"result\",\"MISS\",\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"I\",\"8\"],\"prev\",[\"coord\",[\"J\",\"7\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"H\",\"1\"],\"prev\",[\"coord\",[\"I\",\"5\"],\"prev\",[\"prev\",[\"coord\",[\"H\",\"6\"],\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"C\",\"6\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"C\",\"6\"],\"prev\",[\"prev\",[\"prev\",[\"coord\",[\"E\",\"3\"],\"result\",\"HIT\",\"prev\",[\"result\",\"MISS\",\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"coord\",[\"H\",\"3\"],\"prev\",[\"coord\",[\"D\",\"10\"],\"prev\",[\"prev\",[\"result\",\"MISS\",\"prev\",[\"result\",\"MISS\",\"coord\",[\"E\",\"1\"],\"prev\",[\"coord\",[\"B\",\"1\"],\"prev\",[\"coord\",[\"B\",\"3\"],\"result\",\"MISS\",\"prev\",[\"result\",\"HIT\",\"coord\",[\"J\",\"10\"],\"prev\",[\"result\",\"MISS\",\"coord\",[\"C\",\"10\"],\"prev\",[\"prev\",[\"coord\",[\"J\",\"2\"]],\"result\",\"HIT\",\"coord\",[\"D\",\"7\"]]]]],\"result\",\"MISS\"]],\"coord\",[\"H\",\"3\"]],\"coord\",[\"J\",\"8\"],\"result\",\"HIT\"],\"result\",\"HIT\"],\"result\",\"MISS\"],\"coord\",[\"H\",\"2\"]],\"coord\",[\"F\",\"2\"],\"result\",\"HIT\"],\"coord\",[\"A\",\"3\"]]],\"coord\",[\"C\",\"4\"],\"result\",\"MISS\"],\"coord\",[\"D\",\"3\"],\"result\",\"MISS\"]],\"result\",\"MISS\"],\"coord\",[\"G\",\"9\"]]],\"coord\",[\"A\",\"8\"],\"result\",\"MISS\"],\"result\",\"MISS\"],\"result\",\"MISS\"]],\"result\",\"HIT\"],\"result\",\"MISS\",\"coord\",[\"A\",\"8\"]],\"coord\",[\"I\",\"7\"],\"result\",\"MISS\"]],\"coord\",[\"H\",\"9\"]],\"result\",\"HIT\",\"coord\",[\"A\",\"1\"]],\"coord\",[\"F\",\"10\"]]],\"result\",\"HIT\",\"coord\",[\"J\",\"4\"]],\"result\",\"MISS\",\"coord\",[\"C\",\"9\"]],\"result\",\"MISS\"]],\"coord\",[\"C\",\"2\"],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"B\",\"10\"]]],\"coord\",[\"E\",\"7\"],\"result\",\"MISS\"],\"coord\",[\"C\",\"1\"],\"result\",\"MISS\"]]],\"coord\",[\"H\",\"4\"]],\"coord\",[\"I\",\"9\"],\"result\",\"MISS\"],\"coord\",[\"F\",\"5\"],\"result\",\"HIT\"],\"coord\",[\"I\",\"6\"],\"result\",\"MISS\"],\"result\",\"HIT\",\"coord\",[\"F\",\"9\"]],\"coord\",[\"E\",\"1\"],\"result\",\"MISS\"],\"result\",\"HIT\"],\"result\",\"HIT\",\"coord\",[\"G\",\"8\"]],\"coord\",[\"I\",\"10\"]],\"coord\",[\"C\",\"8\"]],\"result\",\"HIT\",\"coord\",[\"I\",\"3\"]],\"result\",\"MISS\",\"coord\",[\"G\",\"10\"]],\"result\",\"MISS\"],\"coord\",[\"I\",\"1\"]]],\"result\",\"HIT\",\"coord\",[\"E\",\"2\"]],\"coord\",[\"G\",\"9\"]]],\"coord\",[\"C\",\"2\"],\"result\",\"MISS\"]]],\"coord\",[\"J\",\"6\"],\"result\",\"MISS\"]]],\"coord\",[\"E\",\"5\"]],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"G\",\"4\"]],\"coord\",[\"A\",\"10\"],\"result\",\"MISS\"],\"coord\",[\"E\",\"8\"]],\"coord\",[\"F\",\"6\"]]]],\"coord\",[\"F\",\"1\"]],\"result\",\"MISS\"],\"coord\",[\"J\",\"9\"],\"result\",\"MISS\"]],\"result\",\"HIT\"],\"result\",\"MISS\"]]]]]]],\"result\",\"MISS\",\"coord\",[\"C\",\"4\"]],\"result\",\"MISS\"],\"coord\",[\"I\",\"9\"]],\"coord\",[\"J\",\"9\"]]],\"coord\",[\"E\",\"4\"],\"result\",\"MISS\"],\"result\",\"MISS\"],\"result\",\"HIT\",\"coord\",[\"B\",\"4\"]],\"result\",\"MISS\"],\"result\",\"HIT\",\"coord\",[\"B\",\"5\"]],\"coord\",[\"H\",\"7\"]],\"coord\",[\"A\",\"5\"],\"result\",\"MISS\"]],\"coord\",[\"B\",\"7\"],\"result\",\"HIT\"]],\"coord\",[\"B\",\"3\"]],\"coord\",[\"H\",\"5\"],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"D\",\"9\"]]],\"result\",\"MISS\",\"coord\",[\"H\",\"5\"]],\"coord\",[\"D\",\"2\"],\"result\",\"MISS\"]],\"result\",\"MISS\",\"coord\",[\"B\",\"6\"]],\"coord\",[\"B\",\"8\"]],\"coord\",[\"F\",\"4\"],\"result\",\"HIT\"],\"coord\",[\"B\",\"2\"]],\"result\",\"MISS\"]],\"result\",\"MISS\",\"coord\",[\"A\",\"2\"]],\"coord\",[\"D\",\"1\"],\"result\",\"MISS\"],\"coord\",[\"F\",\"10\"]]],\"result\",\"MISS\"],\"result\",\"MISS\"],\"coord\",[\"B\",\"7\"]]],\"result\",\"MISS\"]],\"coord\",[\"G\",\"10\"]],\"result\",\"MISS\",\"coord\",[\"C\",\"3\"]],\"result\",\"MISS\",\"coord\",[\"B\",\"4\"]]]]],\"coord\",[\"C\",\"1\"],\"result\",\"HIT\"],\"coord\",[\"A\",\"4\"],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"I\",\"6\"]]],\"result\",\"MISS\"],\"coord\",[\"E\",\"8\"],\"result\",\"MISS\"],\"coord\",[\"A\",\"5\"],\"result\",\"HIT\"],\"coord\",[\"F\",\"1\"],\"result\",\"MISS\"],\"coord\",[\"H\",\"8\"],\"result\",\"HIT\"],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"G\",\"7\"]],\"result\",\"MISS\"],\"coord\",[\"F\",\"3\"],\"result\",\"MISS\"],\"coord\",[\"H\",\"8\"],\"result\",\"MISS\"]],\"coord\",[\"E\",\"3\"],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"B\",\"5\"]],\"coord\",[\"G\",\"6\"],\"result\",\"MISS\"]]]],\"coord\",[\"E\",\"5\"]],\"result\",\"MISS\",\"coord\",[\"A\",\"10\"]],\"result\",\"HIT\"],\"result\",\"MISS\"],\"result\",\"MISS\"],\"coord\",[\"G\",\"8\"],\"result\",\"MISS\"]],\"result\",\"MISS\",\"coord\",[\"C\",\"7\"]],\"result\",\"MISS\"],\"result\",\"MISS\",\"coord\",[\"H\",\"10\"]],\"result\",\"MISS\",\"coord\",[\"J\",\"3\"]]],\"result\",\"MISS\"]],\"result\",\"MISS\",\"coord\",[\"F\",\"9\"]],\"coord\",[\"H\",\"2\"]],\"result\",\"MISS\",\"coord\",[\"F\",\"3\"]],\"coord\",[\"I\",\"7\"]]]],\"result\",\"MISS\"],\"coord\",[\"H\",\"9\"],\"result\",\"MISS\"],\"coord\",[\"E\",\"10\"],\"result\",\"MISS\"],\"coord\",[\"D\",\"10\"]],\"coord\",[\"J\",\"1\"],\"result\",\"MISS\"],\"coord\",[\"G\",\"2\"],\"result\",\"MISS\"],\"coord\",[\"D\",\"6\"]],\"result\",\"MISS\"],\"coord\",[\"I\",\"2\"]]]]],\"coord\",[\"C\",\"7\"]]],\"coord\",[\"E\",\"6\"]],\"coord\",[\"A\",\"3\"],\"result\",\"MISS\"]],\"coord\",[\"H\",\"1\"]]]]],\"result\",\"MISS\",\"coord\",[\"I\",\"2\"]]],\"coord\",[\"C\",\"3\"],\"result\",\"MISS\"],\"coord\",[\"G\",\"5\"],\"result\",\"MISS\"],\"coord\",[\"J\",\"3\"]]]"