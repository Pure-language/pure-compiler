{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Main where
  import Core.Parser.Parser (parsePure)
  import Text.Parsec.Error (errorMessages, errorPos, messageString)
  import Error.Diagnose.Compat.Parsec
  import Error.Diagnose (addFile, printDiagnostic, stderr, defaultStyle, err, Position (Position), Marker (This), def, stdout, addReport, Note (Note))
  import Data.Void (Void)
  import Core.TypeChecking.Type (runCheck)
  import Text.Parsec.Pos
  import Core.Compiler.Compiler (runCompiler)
  import Core.ANF (runANF)
  import Core.Compiler.CodeGen.Generation (from)
  import Data.Bifunctor (Bifunctor(bimap))
  import Core.Garbage (runGarbageMonad)
  import Core.Conversion.Hoisting (runHoisting)
  import Core.Conversion.Close (runClosureConversion)
  import Core.Import.Resolver (inspectImports)
  
  instance HasHints Void [Char] where
    hints _ = mempty

  main :: IO ()
  main = do
    let file = "tests/example.pure"
    x <- readFile file
    case parsePure file x of
      Right ast -> do
        ast <- inspectImports file ast
        ast' <- runCheck ast
        case ast' of
          Right ast -> do
            ast <- runANF ast
            --ast <- runGarbageMonad ast
            --mapM_ ((>> putStrLn "") . print) ast
          
            c <- runCompiler ast
            let c' = concatMap ((++";") . from) c
            writeFile "test.js" (c' ++ "main();")
          Left (error, msg, (p1, p2)) -> do
            let p1' = (sourceLine p1, sourceColumn p1)
            let p2' = (sourceLine p2, sourceColumn p2)
            let file' = sourceName p1
            x' <- readFile file'
            let pos' = Position p1' p2' $ sourceName p1
            let beautifulExample = err
                  Nothing
                  error
                  [ (pos', This "While typechecking this") ]
                  (maybe [] ((:[]) . Note) msg)

            -- Create the diagnostic 
            let diagnostic  = addFile def file' x'
            let diagnostic' = addReport diagnostic beautifulExample

            -- Print with unicode characters, colors and the default style
            printDiagnostic stdout True True 4 defaultStyle diagnostic'
      Left err ->
        let diag  = errorDiagnosticFromParseError Nothing "Parse error on input" Nothing err
            diag' = addFile diag file x
          in printDiagnostic stderr True True 4 defaultStyle diag'
