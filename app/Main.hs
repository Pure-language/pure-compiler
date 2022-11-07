{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TupleSections #-}
module Main where
  import Core.Parser.Parser (parsePure)
  import Text.Parsec.Error (errorMessages, errorPos, messageString)
  import Error.Diagnose.Compat.Parsec
  import Error.Diagnose (addFile, printDiagnostic, stderr, defaultStyle, err, Position (Position), Marker (This), def, stdout, addReport)
  import Data.Void (Void)
  import Core.TypeChecking.Type (runCheck)
  import Text.Parsec.Pos
  import Core.Compiler.Compiler (runCompiler)
  import Core.ANF (runANF)
  import Core.Compiler.CodeGen.Generation (from)
  import Data.Bifunctor (Bifunctor(bimap))
  import Core.Garbage (runGarbageMonad)
  import Core.TypeChecking.Unification (TypeState(modules), Module (Module))
  import qualified Data.Map as M
  import Control.Arrow (Arrow(second))
  import System.FilePath
  import Core.AsyncChecker (checkToplevel)
  
  main :: IO ()
  main = do
    let file = "tests/tokenizer.pure"
    x <- readFile file
    case parsePure file x of
      Right ast -> do
        case mapM checkToplevel ast of
          Right ast -> do
            ast' <- runCheck ast
            case ast' of
              Right (ast, state) -> do
                ast <- runANF ast
                --ast <- runGarbageMonad ast
                --mapM_ ((>> putStrLn "") . print) ast
                
                cs <- mapM (\(Module path stmts) -> (path,) <$> runCompiler state stmts) $ M.elems (modules state)
                c <- runCompiler state ast
                let cs' = map (second (concatMap ((++ ";") . from))) cs
                let c' = concatMap ((++";") . from) c
                mapM_ (\(path, c) -> writeFile (path -<.> ".mjs") c) cs'
                writeFile (file -<.> ".mjs") (c' ++ "main();")
              err -> printError err "While typechecking this"
          err -> printError err "While checking await expressions"
      Left err ->
        let diag  = errorDiagnosticFromParseError Nothing "Parse error on input" Nothing err
            diag' = addFile diag file x
          in printDiagnostic stderr True True 4 defaultStyle diag'

  printError :: Either (String, Maybe String, (SourcePos, SourcePos)) b -> String -> IO ()
  printError (Left (error, msg, (p1, p2))) step = do
    let p1' = (sourceLine p1, sourceColumn p1)
    let p2' = (sourceLine p2, sourceColumn p2)
    let file' = sourceName p1
    x' <- readFile file'
    let pos' = Position p1' p2' $ sourceName p1
    let beautifulExample = err
          Nothing
          error
          [ (pos', This step) ]
          (maybe [] ((:[])) msg)

    -- Create the diagnostic 
    let diagnostic  = addFile def file' x'
    let diagnostic' = addReport diagnostic beautifulExample

    -- Print with unicode characters, colors and the default style
    printDiagnostic stdout True True 4 defaultStyle diagnostic'
  printError _ _ = return ()