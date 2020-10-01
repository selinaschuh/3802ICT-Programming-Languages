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

    -- validates bool
    boolV :: JSValue -> Bool
    boolV json = if isJSBool json then valid else invalid

    validate :: JSValue -> JSValue -> Bool
    validate _ (JSObject []) = valid -- empty schema matches anything
    validate json (JSObject [JSMember "\"type\"" (JSString "\"bool\"")]) = boolV json
    validate json _ = invalid

    parse :: String -> JSValue
    parse file = 
        let cps = preLex file 
        in case inputL cps of
                Error pos msg -> Prelude.error msg
                OK(tlps, _) ->
                    
                    case jsonP tlps of
                        Error pos msg -> Prelude.error msg
                        OK(json, _) -> json

    


