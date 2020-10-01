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

    validate :: JSValue -> JSValue -> Bool
    validate (JSObject []) _ = valid -- empty schema matches anything
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

    


