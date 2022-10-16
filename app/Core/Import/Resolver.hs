{-# LANGUAGE LambdaCase #-}
module Core.Import.Resolver where
  import Core.Parser.AST
  import System.FilePath (joinPath, takeDirectory, (</>))
  import Core.Parser.Parser (parsePure)
  import Text.Parsec (SourcePos)
  import Data.List (intersperse, intercalate)

  isPublic :: Located Statement -> Bool
  isPublic (Public _ :> _) = True
  isPublic _ = False

  removePublic :: Located Statement -> Located Statement
  removePublic (Public x :> _) = x
  removePublic x = x

  inspectImports :: FilePath -> [Located Statement] -> IO [Located Statement]
  inspectImports file = (concat <$>) . mapM (\case
    Import s :> pos -> map removePublic . filter isPublic <$> resolveImport (takeDirectory file) s
    x -> return [x])

  resolveImport :: FilePath -> [String] -> IO [Located Statement]
  resolveImport dir mod = do
    let mod' = joinPath mod
    let path = dir </> mod' ++ ".pure"
    file <- readFile path
    case parsePure path file of
      Right ast -> inspectImports file ast
      Left e -> do
        fail $ "The module '" ++ mod' ++ "' could not be located" 
  