module JSON.Schema.Validator (
    valid, invalid, parse, validate

) where

    import System.IO
    import System.Environment

    import ABR.Parser
    import JSON.Parser



    invalid :: Bool
    invalid = False

    valid :: Bool
    valid = True


    -- checks if a given JSValue is JSTrue or JSFalse
    isJSBool :: JSValue -> Bool
    isJSBool v = case v of
                    JSTrue -> True
                    JSFalse -> True
                    otherwise -> False

    isInt :: RealFrac a => a -> Bool
    isInt x = x == fromInteger (round x)

    -- validates bool
    boolV :: JSValue -> Bool
    boolV json = if isJSBool json then valid else invalid

    -- validates int
    intV :: JSValue -> Bool
    intV (JSNumber x) = if isInt x then valid else invalid
    intV _ = invalid

    -- validates float
    floatV :: JSValue -> Bool
    floatV (JSNumber x) = valid
    floatV _ = invalid 

    --validates string
    stringV :: JSValue -> Bool
    stringV (JSString s) = valid
    stringV _ = invalid

    validate :: JSValue -> JSValue -> Bool
    validate _ (JSObject []) = valid -- empty schema matches anything
    validate json (JSObject [JSMember "\"type\"" (JSString "\"bool\"")]) = boolV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"int\"")]) = intV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"float\"")]) = floatV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"string\"")]) = stringV json
    validate _ _ = invalid


    parse :: String -> JSValue
    parse file = 
        let cps = preLex file 
        in case inputL cps of
                Error pos msg -> Prelude.error msg
                OK(tlps, _) ->
                    
                    case jsonP tlps of
                        Error pos msg -> Prelude.error msg
                        OK(json, _) -> json

    


