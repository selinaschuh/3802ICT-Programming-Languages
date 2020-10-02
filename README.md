# 3802ICT-Programming-Languages
Assignment of 3802ICT Programming Languages @ Griffith University

### JSON Lexer, Parser and Validator
#### The module JSON.Parser includes functions to parse a .json file using the ABR module
#### The module JSON.Schema.Validator checks a JSON data document against a JSON schema document to see if the data document matches the schema specifications

#### program3.hs
Test driver for JSON.Parser which will print the lexemes, pairs of tokens and finally the parse tree. To run the program with the test.json file:
```runhaskell program3.hs < test.json```

#### program4.hs 
Test driver for JSON.Schema.Validator which takes the file name of a JSON data file and JSON schema file respectively and outputs whether or not the data file is valid according to the schema.
To run the program with two test files from the jsonFiles and schemaFiles folder:
```runhaskell program4.hs jsonFiles/nestedSchemaObject.json jsonSchemas/nestedSchema.json```



