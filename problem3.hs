
module Main (main) where

    import System.IO
    import JSON.Parser
    import ABR.Parser


    main :: IO ()
    main = do
        
        putStr ">> "
        hFlush stdout
        input <- getContents

        -- split input into characters and their positions
        let cps = preLex input
        putStrLn $ "Pairs: " ++ show cps

        -- lex input to get lexemes 
        case inputL cps of
            Error pos msg -> JSON.Parser.error pos msg
            OK(tlps, _) -> do
                putStrLn $ "Lexemes: " ++ show tlps

                -- parse lexemes
                case jsonP tlps of 
                    Error pos msg -> JSON.Parser.error pos msg
                    OK(json, _) -> do
                        putStrLn $ "Result :" ++ show json