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
  
  instance HasHints Void [Char] where
    hints _ = mempty

  main :: IO ()
  main = do
    let file = "tests/example.pure"
    x <- readFile file
    case parsePure file x of
      Right ast -> do
        ast' <- runCheck ast
        case ast' of
          Right ast -> print ast
          Left (error, msg, (p1, p2)) -> do
            let p1' = (sourceLine p1, sourceColumn p1)
            let p2' = (sourceLine p2, sourceColumn p2)
            let pos' = Position p1' p2' $ sourceName p1
            let beautifulExample = err
                  Nothing
                  error
                  [ (pos', This "While typechecking this")]
                  (maybe [] ((:[]) . Note) msg)

            -- Create the diagnostic 
            let diagnostic  = addFile def file x
            let diagnostic' = addReport diagnostic beautifulExample

            -- Print with unicode characters, colors and the default style
            printDiagnostic stdout True True 4 defaultStyle diagnostic'
      Left err ->
        let diag  = errorDiagnosticFromParseError Nothing "Parse error on input" Nothing err
            diag' = addFile diag "tests/example.pure" x
          in printDiagnostic stderr True True 4 defaultStyle diag'
