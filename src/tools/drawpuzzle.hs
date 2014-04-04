{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.BoundingBox
import Diagrams.Backend.CmdLine

import Puzzles.Parse.Puzzle
import Puzzles.Compose
import Puzzles.Diagrams.Draw

import Options.Applicative
import Control.Monad
import Data.Maybe

import System.FilePath
import System.Environment (getProgName)
import System.Exit

import qualified Data.Yaml as Y
import Data.Aeson (Result(..))
import Data.Aeson.Types (parse)

data PuzzleOpts = PuzzleOpts
    { _format   :: String
    , _puzzle   :: Bool
    , _solution :: Bool
    , _example  :: Bool
    , _input    :: FilePath
    }

puzzleOpts :: Parser PuzzleOpts
puzzleOpts = PuzzleOpts
    <$> strOption
            (long "format" <> short 'f'
             <> value "png"
             <> metavar "FMT"
             <> help "Desired output format by file extension")
    <*> switch
            (long "puzzle" <> short 'p'
             <> help "Render puzzle (to base.ext")
    <*> switch
            (long "solution" <> short 's'
             <> help "Render solution (to base-sol.ext)")
    <*> switch
            (long "example" <> short 'e'
             <> help "Render example (to base.ext)")
    <*> argument str
            (metavar "INPUT"
             <> help "Puzzle file in .pzl format")

instance Parseable PuzzleOpts where
    parser = puzzleOpts

cmtopoint :: Double -> Double
cmtopoint = (* 28.3464567)

outputSuffix DrawPuzzle = ""
outputSuffix DrawSolution = "-sol"
outputSuffix DrawExample = ""

toDiagramOpts :: OutputChoice -> Double -> PuzzleOpts -> DiagramOpts
toDiagramOpts oc w (PuzzleOpts f _ _ _ i) =
    DiagramOpts (Just w') Nothing out
    where w' = case f of "png" -> round (40 * w)
                         _     -> round . cmtopoint $ (0.8 * w)
          base = takeBaseName i
          out = addExtension (base ++ outputSuffix oc) f

renderPuzzle :: PuzzleOpts -> (OutputChoice -> Maybe (Diagram B R2)) ->
                (OutputChoice, Bool) -> IO ()
renderPuzzle opts r (oc, req) = do
    let x = r oc
    if req && isNothing x
        then exitErr ("failed to render (no solution?): " ++ show oc)
        else return ()
    when (isJust x) $ do
        let Just x' = x
            w = fst . unr2 . boxExtents . boundingBox $ x'
            dopts = toDiagramOpts oc w opts
            lopts = DiagramLoopOpts False Nothing 0
        mainRender (dopts, lopts) x'

defaultOpts :: Parser a -> IO a
defaultOpts optsParser = do
    prog <- getProgName
    let p = info (helper <*> optsParser)
                (fullDesc
                 <> progDesc "Command-line diagram generation."
                 <> header prog)
    execParser p

checkOutput :: PuzzleOpts -> IO [(OutputChoice, Bool)]
checkOutput (PuzzleOpts _ p s e _)
    | (p || s) && e  = exitErr "example output conflicts with puzzle/solution"
    | e              = return . map req $ [DrawExample]
    | p && s         = return . map req $ [DrawPuzzle, DrawSolution]
    | p              = return . map req $ [DrawPuzzle]
    | s              = return . map req $ [DrawSolution]
    | otherwise      = return [req DrawPuzzle, opt DrawSolution]
  where
    req x = (x, True)
    opt x = (x, False)

readPuzzle :: FilePath -> IO (Maybe TypedPuzzle)
readPuzzle = Y.decodeFile

exitErr :: String -> IO a
exitErr e = putStrLn e >> exitFailure

main = do
    opts <- defaultOpts puzzleOpts
    ocs <- checkOutput opts
    mp <- readPuzzle (_input opts)
    p <- case mp of Nothing -> exitErr "failed to parse yaml"
                    Just p  -> return p
    let TP t pv msv = p
        ps = parse (handle drawPuzzleMaybeSol fail t) (pv, msv)
    case ps of Success ps' -> mapM_ (renderPuzzle opts (draw ps')) ocs
               Error e -> exitErr e
