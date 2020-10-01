module JSON.Schema.Validator where

    import Data.List
    import Data.Char
    import System.IO
    import System.Environment

    import ABR.Util.Pos
    import ABR.Parser
    import ABR.Parser.Lexers

    type JsNumber = Double
    type JsString = String
    type JsMemberName = String

    data JSMember = JSMember JsMemberName JSValue deriving (Show)

    data JSValue 
        = JSNumber JsNumber
        | JSTrue
        | JSFalse 
        | JSString JsString
        | JSArray [JSValue]
        | JSObject [JSMember]
        deriving (Show)


    invalid :: Bool
    invalid = False

    valid :: Bool
    valid = True
   

   -- Helper Functions
    isDigitNotZero :: Char -> Bool
    isDigitNotZero c | c > '0' && c <= '9'    = True
                     | otherwise              = False
                  

    -- Lexers

    -- Lexer for the entire input
    inputL :: Lexer
    inputL = dropWhite $ nofail $ total $ listL
            [whitespaceL, symbolL, boolL, stringL, numberL]
   
    symbolL :: Lexer
    symbolL = literalL '{' <|> literalL '}' <|>
             literalL '[' <|> literalL ']' <|>
             literalL ':' <|> literalL ','

    boolL :: Lexer
    boolL = (tokenL "true" <|> tokenL "false") %> "bool"

    digitNotZeroL :: Lexer
    digitNotZeroL = satisfyL (\d -> isDigitNotZero d) "digitNotZero"

    digitL :: Lexer
    digitL = satisfyL (\d -> d == '0') "" <|> digitNotZeroL %> "digit"

    numberL :: Lexer
    numberL = soft (optional (literalL '-')) <&&>                                    -- optional '-' and
              (literalL '0' <|> (digitNotZeroL <&&>  (many digitL &%> ""))) <&&>     -- 0 or (digit1to9 and many digits) and
              soft (optional (literalL '.' <&&> (many digitL &%> ""))) <&&>          -- optional (. and at least one digit) 
              soft (optional ((literalL 'e' <|> literalL 'E') <&&>                   -- optional e or E and 
                               soft (optional (literalL '+' <|> literalL '-')) <&&>  -- optional - or + and
                               (some digitL &%> ""))) %> "number"                    -- one or more digits

    -- Parsers

    jsBoolP :: Parser JSValue
    jsBoolP = tagP "bool" @> (\ (_, bool, _) -> 
                                 case bool of 
                                    "true" -> JSTrue
                                    "false" -> JSFalse)

    jsNumberP :: Parser JSValue
    jsNumberP = tagP "number" @> (\ (_, n, _) -> JSNumber (read n))

    jsStringP :: Parser JSValue
    jsStringP = tagP "string" @> (\ (_, s, _) -> JSString s)

    jsArrayP :: Parser JSValue
    jsArrayP = tagP "'['" &>
                    (optional jsValueP <&> many (tagP "','" &> jsValueP) @> (\ (v, vs) -> v ++ vs)) -- join the two lists of values
                <& tagP "']'" @> JSArray

    jsMemberP :: Parser JSMember
    jsMemberP = (tagP "string" <&> tagP "':'" &> jsValueP)  @> (\((_,name,_), val) -> JSMember name val)

    -- jsObejectP :: Parser JSValue
    jsObejectP = (tagP "'{'" &>                                                  -- {
                    ((optional jsMemberP) <&>                                    -- optional (member and
                     many (tagP "','" &> jsMemberP) @> (\ (m, ms) -> m ++ ms))   -- 0 or more members) -> join the two lists
                <& tagP "'}'") @> JSObject                                       -- }


    jsValueP :: Parser JSValue
    jsValueP = jsBoolP <|> jsNumberP <|> jsStringP <|> jsArrayP <|> jsObejectP

    jsonP = nofail $ total $ jsValueP 


    -- custom error reporting function
    error :: Pos -> Msg -> IO ()
    error (line,col) msg = do
            putStrLn $ "Error on line " ++ show line ++ 
                       " and column " ++ show col ++ 
                       " : " ++ msg

    parse :: String -> JSValue
    parse file = 
        let cps = preLex file 
        in case inputL cps of
                Error pos msg -> Prelude.error msg
                OK(tlps, _) ->
                    
                    case jsonP tlps of
                        Error pos msg -> Prelude.error msg
                        OK(json, _) -> json

    
    validate :: JSValue -> JSValue -> Bool
    validate (JSObject []) _ = valid -- empty schema matches anything
    validate _ _ = invalid



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
        putStrLn $ show json
        putStrLn $ show schema

        let isValid = validate schema json
        let result = if isValid then "Valid" else "Not Valid"
        putStrLn result
        putStrLn $ show isValid