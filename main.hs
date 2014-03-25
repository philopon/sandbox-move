{-# LANGUAGE LambdaCase, OverloadedStrings #-}

import System.Environment
import System.Directory
import System.FilePath
import System.Exit
import System.IO
import System.Process

import Control.Monad
import Control.Applicative

import Data.Monoid

import qualified Data.Attoparsec as P
import qualified Data.Attoparsec.Char8 as PC
import qualified Data.ByteString.Char8 as SC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Builder as B

canonicalize :: FilePath -> FilePath
canonicalize = normalise . joinPath . go . splitPath . normalise
  where go (_:p:ps) | p `elem` up = go ps
        go (p:ps)                 = p:go ps
        go []                     = []
        up = ".." : map (\c -> ".." ++ [c]) pathSeparators

main :: IO ()
main = getArgs >>= \case
    [from, to] -> do
        cd <- getCurrentDirectory
        doesFileExist (from </> "cabal.sandbox.config") >>= \e ->
            unless e (hPutStrLn stderr "cabal.sandbox.config not found." >> exitFailure)
        P.parseOnly packageDB <$> SC.readFile (from </> "cabal.sandbox.config") >>= \case
            Left e -> hPutStrLn stderr e >> exitFailure
            Right Nothing -> hPutStrLn stderr "package-db: entry not found." >> exitFailure
            Right (Just pdb) -> do
                changeCabalConfig (canonicalize $ cd </> from) (canonicalize $ cd </> to)
                (filter ((== ".conf") . takeExtension) <$> getDirectoryContents pdb) >>= mapM_ (\conf -> do
                    changeConfig (canonicalize $ cd </> from) (canonicalize $ cd </> to)  (pdb </> conf)
                    )
                renameDirectory from to
        setCurrentDirectory to
        void $ rawSystem "cabal" ["sandbox", "hc-pkg", "--", "recache", "--user"]

    _ -> getProgName >>= \pn -> hPutStrLn stderr ("USAGE: " ++ pn ++ " from to") >> exitFailure

globalReplaceP :: FilePath -> FilePath -> P.Parser L.ByteString
globalReplaceP from to = B.toLazyByteString . mconcat <$> many ((B.stringUtf8 to <$ P.string (SC.pack from)) <|> B.word8 <$> P.anyWord8)

packageDB :: P.Parser (Maybe FilePath)
packageDB = fmap SC.unpack . mconcat <$> many (Just <$> (PC.stringCI "package-db:" *> PC.skipSpace *> P.takeWhile (\c -> not $ PC.isEndOfLine c || PC.isHorizontalSpace c)) <|> (Nothing <$ P.anyWord8))

changeCabalConfig :: FilePath -> FilePath -> IO ()
changeCabalConfig from to = changeConfig from to (from </> "cabal.sandbox.config")

changeConfig :: FilePath -> FilePath -> FilePath -> IO ()
changeConfig from to file =
    P.parseOnly (globalReplaceP from to) <$> SC.readFile file >>= \case
        Right r -> L.writeFile file r
        Left  _ -> return ()
