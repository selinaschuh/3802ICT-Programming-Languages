
module JSON where

    import Data.List
    import Data.Char
    import System.IO

    import ABR.Util.Pos
    import ABR.Parser
    import ABR.Parser.Lexers

    -- type Key = String
    -- type Array = [Value]



    -- -- one key value pair of an object
    -- type Member = (Key, Value)
    -- type Object = [Member]

    -- type JSONBool = Bool
    -- type JSONString = String
    -- type JSONNumber = Double

    -- type JSONArray = [JSONValue]
    -- type JSONObject = [(String, JSONValue)]

    -- data JSONValue = JSONBool | JSONNumber | JSONString | JSONArray | JSONObject
    --                  deriving (Show, Eq)

    data JsValue
        = JsNull
        | JsBool Bool
        | JsNumber Double
        | JsString String
        | JsArray [JsValue]
        | JsObject [(String, JsValue)]
        deriving (Show, Eq)

    
    -- LEXERS
    -- use stringL, whitespaceL, floatL

    symbolL :: Lexer
    symbolL = literalL '(' <|> literalL ')' <|>
              literalL '[' <|> literalL ']' <|> 
              literalL '{' <|> literalL '}' <|> 
              literalL ',' <|> literalL ':' <|> literalL ';'
    
    -- lexer for boolean value True or False 
    jsonBoolL :: Lexer 
    jsonBoolL = (tokenL "True" <|> tokenL "False") %> "JSONBool"

    -- lexer for JSONString
    jsonStringL :: Lexer
    jsonStringL = stringL %> "JSONString"

    jsonNumberL :: Lexer
    jsonNumberL = floatL %> "JSONNumber"

    jsonValueL :: Lexer
    jsonValueL = jsonStringL <|> jsonBoolL %> "JSONValue"

    -- an array starts with '[' followed by 0 to more values followed by ']'
    jsonArrayL :: Lexer 
    jsonArrayL = literalL '[' <&&> soft (optional jsonStringL) <&&>
                 literalL ']' %> "JSONArray"

    inputL :: Lexer
    inputL = dropWhite $ nofail $ total $ listL 
             [whitespaceL, jsonBoolL, jsonStringL, jsonNumberL, symbolL]


    jsonBoolP :: Parser JsValue
    jsonBoolP = tagP "JSONBool" @> (\ (_, bool, _) -> 
                                        case bool of 
                                            "True" -> JsBool True
                                            "False" -> JsBool False)

    jsonP :: Parser JsValue
    jsonP = nofail $ total $ jsonBoolP

    -- jsonP :: Parser JSONBool
    -- jsonP = nofail $ total jsonBoolP

    error :: Pos -> Msg -> IO ()
    error (line,col) msg = do
            putStrLn $ "Error on line " ++ show line ++ 
                       " and column " ++ show col ++ 
                       " : " ++ msg


    main :: IO ()
    main = do
        
        putStr ">> "
        hFlush stdout
        input <- getLine
    
        let cps = preLex input
        putStrLn $ "Pairs: " ++ show cps

        case inputL cps of
            Error pos msg -> JSON.error pos msg
            OK(tlps, _) -> do
                putStrLn $ "Lexemes: " ++ show tlps

                case jsonP tlps of 
                    Error pos msg -> JSON.error pos msg
                    OK(json, _) -> do
                        putStrLn $ "Result :" ++ show json
                main



