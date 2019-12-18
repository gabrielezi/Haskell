{-# LANGUAGE OverloadedStrings #-}
module Main where

    import Parser
    import Logic

    import Network.Wreq
    import Control.Lens
    import System.Random
    import qualified Data.ByteString.Lazy.Char8 as B

    main :: IO ()
    main = do
      putStrLn "Enter game Id"
      gameId <- getLine
      putStrLn "attack or defence?"
      v <- getLine
      case v of
        "attack" -> attack gameId
        "defence" -> defence gameId "B"
        _ -> putStrLn "wrong choice"

    attack :: String -> IO()
    attack gameId = do
        let s :: String
            s = "[\"coord\",[\"A\",\"1\"]]"
        let opts = defaults & header "Content-type" .~ ["application/json+nomaps"]
        postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/A") $ B.pack s
        defence gameId "A"
    
    defence :: String -> String -> IO()
    defence gameId player = do
        let opts = defaults & header "Accept" .~ ["application/json+nomaps"]
        r <- getWith opts  ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player)
        case r ^. responseStatus . statusCode of
            200 -> do        
                let mov :: String
                    mov = B.unpack (r ^. responseBody)
                putStrLn ("From request: " ++ mov)
                case parse mov initialMsg of
                    Left e -> putStrLn ("parser error: " ++ e)
                    Right(msg, _) -> case response msg of
                        Left mess -> case mess of
                            'W' : 'o' : 'n' : rest -> do
                                putStrLn ("Won: " ++ rest)
                                putStrLn "You are the winner!"
                            'L' : 'o' : 's' : 't' : rest -> do
                                putStrLn ("Lost: " ++ rest)
                                let opts = defaults & header "Content-type" .~ ["application/json+nomaps"]
                                postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player) $ B.pack rest
                                putStrLn "You lost :/"
                            error -> putStrLn error
                        Right stuff -> do
                            putStrLn("response is: " ++ stuff)
                            let opts = defaults & header "Content-type" .~ ["application/json+nomaps"]
                            postWith opts ("http://battleship.haskell.lt/game/" ++ gameId ++ "/player/" ++ player) $ B.pack stuff
                            defence gameId player  
            _ -> putStrLn $ "http request failed (return code " ++ show (r ^. responseStatus . statusCode) ++ ")" 



