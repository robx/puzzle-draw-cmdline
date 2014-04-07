{-# LANGUAGE FlexibleContexts #-}

module Main where

import Diagrams.Prelude hiding (value, option, (<>), Result)
import Diagrams.Backend.Cairo.CmdLine
import Diagrams.BoundingBox
import Diagrams.Backend.CmdLine

import Text.Puzzles.Puzzle
import Data.Puzzles.Compose
import Diagrams.Puzzles.Draw
import Data.Puzzles.PuzzleTypes

import Options.Applicative
import Control.Monad
import Data.Maybe

import System.FilePath
import System.Environment (getProgName)
import System.Exit

import qualified Data.Yaml as Y

data PuzzleOpts = PuzzleOpts
    { _format   :: String
    , _type     :: Maybe String
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
    <*> (optional . strOption $
            (long "type" <> short 't'
             <> metavar "TYPE"
             <> help "Puzzle type, overriding type in input file"))
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

outputSuffix :: OutputChoice -> String
outputSuffix DrawPuzzle = ""
outputSuffix DrawSolution = "-sol"
outputSuffix DrawExample = ""

toDiagramOpts :: OutputChoice -> Double -> PuzzleOpts -> DiagramOpts
toDiagramOpts oc w opts =
    DiagramOpts (Just w') Nothing out
  where
    f = _format opts
    w' = case f of "png" -> round (40 * w)
                   _     -> round . cmtopoint $ (0.8 * w)
    base = takeBaseName (_input opts)
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
checkOutput opts
    | (p || s) && e  = exitErr "example output conflicts with puzzle/solution"
    | e              = return . map req $ [DrawExample]
    | p && s         = return . map req $ [DrawPuzzle, DrawSolution]
    | p              = return . map req $ [DrawPuzzle]
    | s              = return . map req $ [DrawSolution]
    | otherwise      = return [req DrawPuzzle, opt DrawSolution]
  where
    p = _puzzle opts
    s = _solution opts
    e = _example opts
    req x = (x, True)
    opt x = (x, False)

checkType :: Maybe String -> IO PuzzleType
checkType mt = do
    t <- maybe errno return mt
    maybe (errunk t) return (lookupType t)
  where
    errno    = exitErr $ "no puzzle type given"
    errunk t = exitErr $ "unknown puzzle type: " ++ t

readPuzzle :: FilePath -> IO (Either Y.ParseException TypedPuzzle)
readPuzzle = Y.decodeFileEither

exitErr :: String -> IO a
exitErr e = putStrLn e >> exitFailure

main :: IO ()
main = do
    opts <- defaultOpts puzzleOpts
    ocs <- checkOutput opts
    mp <- readPuzzle (_input opts)
    TP mt pv msv <- case mp of Left  e -> exitErr $
                                          "parse failure: " ++ show e
                               Right p -> return p
    t <- checkType $ _type opts `mplus` mt
    let ps = Y.parseEither (handle drawPuzzleMaybeSol t) (pv, msv)
    case ps of Right ps' -> mapM_ (renderPuzzle opts (draw ps')) ocs
               Left    e -> exitErr e
