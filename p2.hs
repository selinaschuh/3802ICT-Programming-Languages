
module JSON where

    import Data.List
    import System.IO

    import ABR.Util.Pos
    import ABR.Parser
    import ABR.Parser.Lexers

    -- type Key = String
    -- type Array = [Value]



    -- -- one key value pair of an object
    -- type Member = (Key, Value)
    -- type Object = [Member]

    type JSONBool = Bool
    type JSONNumber = Double
    type JSONString = String

    type JSONArray = [JSONValue]
    type JSONObject = [(String, JSONValue)]

    data JSONValue = JSONBool | JSONNumber | JSONString | JSONArray | JSONObject
                     deriving (Show, Eq)

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
            

    inputL :: Lexer
    inputL = dropWhite $ nofail $ total $ listL 
             [whitespaceL, symbolL, floatL, jsonBoolL, jsonStringL]

    main :: IO ()
    main = do
        
        putStr ">> "
        hFlush stdout
        input <- getLine
        
        let error :: Pos -> Msg -> IO ()
            error (line,col) msg = do
            putStrLn $ "Error on line " ++ show line ++ 
                       " and column " ++ show col ++ 
                       " : " ++ msg
            main
        let cps = preLex input
        putStrLn $ "Pairs: " ++ show cps

        case inputL cps of
            Error pos msg -> error pos msg
            OK(tlps, _) -> do
                putStrLn $ "Lexemes: " ++ show tlps