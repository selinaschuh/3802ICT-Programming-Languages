module Main (main) where

    import System.IO
    import System.Environment
    import JSON.Schema.Validator
    
    main :: IO ()
    main = do
        
        -- get the names of the files
        fnames <- getArgs
        let f1 = fnames !! 0
        let f2 = fnames !! 1
    

        -- read file to be validated input
        handle <- openFile f1 ReadMode  
        jsonIn <- hGetContents handle  

        -- read schema file
        handle' <- openFile f2 ReadMode
        schemaIn <- hGetContents handle'

        -- parse the json file to be validated
        let json = parse jsonIn
        let schema = parse schemaIn
        -- putStrLn $ show json
        -- putStrLn $ show schema

        let isValid = validate json schema
        putStrLn $ "Valid: " ++ show isValid