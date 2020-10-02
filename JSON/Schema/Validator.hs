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


    -- HELPER FUNCTIONS:

    -- checks if a given JSValue is JSTrue or JSFalse
    isJSBool :: JSValue -> Bool
    isJSBool v = case v of
                    JSTrue -> True
                    JSFalse -> True
                    otherwise -> False

    isInt :: RealFrac a => a -> Bool
    isInt x = x == fromInteger (round x)

    -- checks if list of JSMembers contains nested schemas
    hasSchema :: [JSMember] -> Bool
    hasSchema [] = invalid
    hasSchema (JSMember "\"schemas\"" schema : members) = valid
    hasSchema (m:members) = hasSchema members 

    -- recursively iterates through list of JSMembers to extract any schemas it may contain
    getSchemas :: [JSMember] -> [JSMember]
    getSchemas ((JSMember "\"schemas\"" (JSObject members)):_) = members
    getSchemas ((JSMember name value):members) = getSchemas members
    getSchemas [] = []

    -- checks if there's a schema in the list of schemas that matches the given name
    findRightSchema name [] = JSFalse
    findRightSchema name ((JSMember name' value):schemas) | name == name'    = value
                                                          | otherwise        = findRightSchema name schemas

    -- VALIDATOR FUNCTIONS

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

    -- validates array with no type specification
    arrayV :: JSValue -> Bool
    arrayV (JSArray _) = valid
    arrayV _ = invalid

    -- validates array with type specifications
    arrayV' :: JSValue -> JSValue ->  Bool
    arrayV' _ (JSArray []) = valid
    arrayV' (JSObject []) _ = invalid
    --arrayV' object (JSArray (e:elems)) = validate e object && arrayV' object (JSArray elems)
    arrayV' (JSObject (m:members)) (JSArray (e:elems)) = (validate e (JSObject [m]) || arrayV' (JSObject members) (JSArray [e])) && arrayV' (JSObject (m:members)) (JSArray elems)
    arrayV' _ _ = invalid

    -- validates object 
    objectV :: [JSMember] -> [JSMember] -> Bool
    objectV [] [] = valid
    objectV [] sm = invalid
    objectV jm [] = invalid
    objectV (jm:jms) (sm:sms) = memberV jm sm && objectV jms sms

    -- validates object with nested schemas
    objectV' :: [JSMember] -> [JSMember] -> [JSMember] -> Bool
    objectV' [] [] _ = valid
    objectV' [] ((JSMember "\"schemas\"" smValue):_) _ = valid
    objectV' [] sm _ = invalid
    objectV' jm [] _ = invalid
    objectV' (jm:jms) (sm:sms) schemas = memberV' jm sm schemas && objectV' jms sms schemas

    -- validates a json member against a schema member specification
    memberV :: JSMember -> JSMember -> Bool
    memberV (JSMember jmName jmValue) (JSMember smName smValue) | jmName == smName    = validate jmValue smValue    -- if member name matches, validate the value
                                                                | otherwise           = invalid                     -- member names don't match -> invalid

    -- validates a json member against a list of schema member specifications to see if it matches one of them
    memberV' :: JSMember -> JSMember -> [JSMember] -> Bool
    memberV' (JSMember jmName jmValue) (JSMember smName smValue) schemas | jmName == smName    = validate' jmValue smValue schemas    -- if member name matches, validate the value against schemas
                                                                         | otherwise           = invalid    

    -- calls validate after finding the right schema from the list of schemas
    validate' :: JSValue -> JSValue -> [JSMember] -> Bool
    validate' json (JSObject [JSMember "\"type\"" (JSString customSchemaName)]) schemas = 
                                if (schema == JSFalse) then invalid else validate json schema 
                                where schema = findRightSchema customSchemaName schemas
    validate' _ _ _ = invalid

    -- validate json schema -> valid/invalid
    validate :: JSValue -> JSValue -> Bool
    validate _ (JSObject []) = valid -- empty schema matches anything
    validate json (JSObject [JSMember "\"type\"" (JSString "\"bool\"")]) = boolV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"int\"")]) = intV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"float\"")]) = floatV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"string\"")]) = stringV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"array\"")]) = arrayV json
    validate json (JSObject [JSMember "\"type\"" (JSString "\"array\""), JSMember "\"elements\"" object]) = arrayV' object json
    validate (JSObject jMembers) (JSObject (JSMember "\"type\"" (JSString "\"object\"") : sMembers)) = objectV jMembers sMembers || (if (hasSchema sMembers) then (objectV' jMembers sMembers (getSchemas sMembers)) else invalid)
    validate _ _ = invalid

    -- calls JSON.Parser functions to parse the two input files before validating
    parse :: String -> JSValue
    parse file = 
        let cps = preLex file 
        in case inputL cps of
                Error pos msg -> Prelude.error msg
                OK(tlps, _) ->
                    
                    case jsonP tlps of
                        Error pos msg -> Prelude.error msg
                        OK(json, _) -> json

    


