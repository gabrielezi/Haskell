module Logic where

    import Data.List
    import Data.Char
    import Message
    
    myMsg :: Msg
    myMsg = Msg {coord = ("A","2"), result = "HIT", prev = Msg {coord = ("A","9"), result = "HIT", prev = Msg {coord = ("A","10"), result = "HIT", prev =Msg {coord = ("A","8"), result = "", prev = Empty}}}}
    
    shipsMsg = "[\"coord\",[\"C\",\"3\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"C\",\"2\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"2\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"A\",\"2\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"E\",\"4\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"F\",\"5\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"E\",\"5\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"D\",\"5\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"B\",\"9\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"B\",\"8\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"A\",\"9\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"A\",\"8\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"J\",\"5\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"I\",\"5\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"I\",\"4\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"H\",\"4\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"I\",\"9\"],\"result\",\"MISS\",\"prev\",[\"coord\",[\"H\",\"9\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"G\",\"9\"],\"result\",\"HIT\",\"prev\",[\"coord\",[\"F\",\"9\"]]]]]]]]]]]]]]]]]]]]]"

    -- +--+-+-+-+-+-+-+-+-+-+-+    
    -- |  |A|B|C|D|E|F|G|H|I|J|    
    -- +--+-+-+-+-+-+-+-+-+-+-+    
    -- | 1| | | | | |x|x|x|x| |    
    -- +--+-+-+-+-+-+-+-+-+-+-+    
    -- | 2| | | | | | | | | | |    
    -- +--+-+-+-+-+-+-+-+-+-+-+    
    -- | 3| |x| | | | | | | | |    
    -- +--+-+-+-+-+-+-+-+-+-+-+    
    -- | 4|x|x|x| | | | | | | |    
    -- +--+-+-+-+-+-+-+-+-+-+-+    
    -- | 5| | | | | | | | | | |    
    -- +--+-+-+-+-+-+-+-+-+-+-+   
    -- | 6| | |x|x|x| | | | | |  
    -- +--+-+-+-+-+-+-+-+-+-+-+   
    -- | 7| | | | |x| | | |x|x|   
    -- +--+-+-+-+-+-+-+-+-+-+-+   
    -- | 8|x|x| | | | | | |x|x|   
    -- +--+-+-+-+-+-+-+-+-+-+-+  
    -- | 9| |x|x| | | | | | | |   
    -- +--+-+-+-+-+-+-+-+-+-+-+  
    -- |10| | | | | | | | | | |   
    -- +--+-+-+-+-+-+-+-+-+-+-+  

    shipsOnBoard :: [(String, String)]
    shipsOnBoard = [("F","1"), ("G","1"), ("H","1"), ("I","1"),
                        ("A","8"), ("B","8"), ("B","9"), ("C","9"),
                        ("I","7"), ("J","7"), ("I","8"), ("J","8"),
                        ("A","4"), ("B","4"), ("C","4"), ("B","3"),
                        ("E","6"), ("C","6"), ("D","6"), ("E","7")]
    
    fullBoard :: [(String, String)]
    fullBoard = [("A","1"), ("B","1"), ("C","1"), ("D","1"), ("E","1"), ("F","1"), ("G","1"), ("H","1"), ("I","1"), ("J","1"),
                    ("A","2"), ("B","2"), ("C","2"), ("D","2"), ("E","2"), ("F","2"), ("G","2"), ("H","2"), ("I","2"), ("J","2"),
                    ("A","3"), ("B","3"), ("C","3"), ("D","3"), ("E","3"), ("F","3"), ("G","3"), ("H","3"), ("I","3"), ("J","3"),
                    ("A","4"), ("B","4"), ("C","4"), ("D","4"), ("E","4"), ("F","4"), ("G","4"), ("H","4"), ("I","4"), ("J","4"),
                    ("A","5"), ("B","5"), ("C","5"), ("D","5"), ("E","5"), ("F","5"), ("G","5"), ("H","5"), ("I","5"), ("J","5"),
                    ("A","6"), ("B","6"), ("C","6"), ("D","6"), ("E","6"), ("F","6"), ("G","6"), ("H","6"), ("I","6"), ("J","6"),
                    ("A","7"), ("B","7"), ("C","7"), ("D","7"), ("E","7"), ("F","7"), ("G","7"), ("H","7"), ("I","7"), ("J","7"),
                    ("A","8"), ("B","8"), ("C","8"), ("D","8"), ("E","8"), ("F","8"), ("G","8"), ("H","8"), ("I","8"), ("J","8"),
                    ("A","9"), ("B","9"), ("C","9"), ("D","9"), ("E","9"), ("F","9"), ("G","9"), ("H","9"), ("I","9"), ("J","9"),
                    ("A","10"), ("B","10"), ("C","10"), ("D","10"), ("E","10"), ("F","10"), ("G","10"), ("H","10"), ("I","10"), ("J","10")]
    
    initialMsg :: Msg
    initialMsg = Msg ("0", "0") "" Empty
    
    -- ======================================================================
    checkIfMovesValid :: Msg -> [String] -> [String] -> String -> Bool  -- patikrina ar kiekvienas langelis turi max viena ejima
    checkIfMovesValid (Msg (c1, c2) _ Empty) usedA usedB switch =
        if switch == "A"
            then
                if (c1 ++ c2) `elem` usedA
                then False
                else True
            else
                if (c1 ++ c2) `elem` usedB
                then False
                else True
    checkIfMovesValid (Msg (c1, c2) _ prev) usedA usedB switch =
        if switch == "A"
        then
            if (c1 ++ c2) `elem` usedA
            then False
            else checkIfMovesValid prev (usedA ++ [c1 ++ c2]) usedB "B"
        else
            if (c1 ++ c2) `elem` usedB
            then False
            else checkIfMovesValid prev usedA (usedB ++ [c1 ++ c2]) "A"
    
    
    countScore :: Msg -> Either String (Int, Int)
    countScore msg = if checkIfMovesValid msg [] [] "A"
        then calculate msg 0 0 "" "A"
        else Left "Duplicated attack move exists"
            where
                calculate :: Msg -> Int -> Int -> String -> String -> Either String (Int, Int)
                calculate (Msg coords _ Empty) playerA playerB res switch =
                    if switch == "A"
                    then
                        if res == "HIT"
                        then Right ((playerA + 1), playerB)
                        else Right (playerA, playerB)
                    else
                        if res == "HIT"
                        then Right (playerA, (playerB+1))
                        else Right (playerA, playerB)
                calculate (Msg coords result prev) playerA playerB res switch =
                    if switch == "A"
                    then
                        if res == "HIT"
                        then calculate prev (playerA + 1) playerB result "B"
                        else calculate prev playerA  playerB result "B"
                    else
                        if res == "HIT"
                        then calculate prev playerA (playerB+1) result "A"
                        else calculate prev playerA playerB result "A"
    

    isHit :: [(String, String)] -> (String, String) -> Bool
    isHit ships coord = if coord `elem` ships then True else False
    
    yes = Msg{coord = ("A","2"), result = "", prev = Empty}
    yes2 = Msg{coord = ("A","3"), result = "HIT", prev = Msg {coord = ("A","1"), result = "", prev = Empty}}

    move :: Msg -> Either String Msg
    move msg = Right (Msg (makeMove fullBoard (myUsedMoves msg)) (if isHit shipsOnBoard (getCoord msg) then "HIT" else "MISS") msg)
        where 
            makeMove :: [(String, String)] -> [(String, String)] -> (String, String)
            makeMove board [] = head board
            makeMove board used = makeMove (delete (head used) board) (delete (head used) used)
            
    getCoord :: Msg -> (String, String)
    getCoord (Msg coord _ _) = coord

    encode :: Msg -> String
    encode (Msg ("0","0") result prev) = "[\"coord\",[],\"result\",\"" ++ result ++"\",\"prev\"," ++ (encode prev) ++ "]"
    encode (Msg (coord1, coord2) result Empty) = "[\"coord\",[\"" ++ coord1 ++"\",\"" ++ coord2 ++ "\"]]"
    encode (Msg (coord1, coord2) result prev) = "[\"coord\",[\"" ++ coord1 ++ "\",\"" ++ coord2 ++ "\"],\"result\",\"" ++ result ++"\",\"prev\"," ++ (encode prev) ++ "]"
    
    myUsedMoves :: Msg -> [(String, String)]
    myUsedMoves msg = myUsedMoves' msg [] 0
        where
            myUsedMoves' (Msg (coord1, coord2) result Empty) usedMoves 1 = (usedMoves ++ [(coord1, coord2)])
            myUsedMoves' (Msg _ _ Empty) usedMoves 0 = usedMoves
            myUsedMoves' (Msg (coord1, coord2) result prev) usedMoves 1 = myUsedMoves' prev (usedMoves ++ [(coord1, coord2)]) 0
            myUsedMoves' (Msg _ _ prev) usedMoves 0 = myUsedMoves' prev usedMoves 1
            
    response :: Msg -> Either String String
    response msg = case countScore msg of
        Left e -> Left e
        Right (20, _) -> Left ("Lost" ++ (encode msg))
        Right (_, 20) -> Left ("Won" ++ (encode (Msg ("0", "0") (if isHit shipsOnBoard (getCoord msg) then "HIT" else "MISS") msg)))
        Right (_, _) -> case move msg of
            Left e -> Left e
            Right rMsg -> case countScore rMsg of
                Left e -> Left e
                Right (_, 20) -> Left ("Lost" ++ (encode (Msg ("0", "0") "HIT" msg)))
                Right (20, _) -> Left ("Won" ++ (encode (Msg ("0", "0") (if isHit shipsOnBoard (getCoord msg) then "HIT" else "MISS") msg)))
                Right (_, _) -> Right (encode rMsg)