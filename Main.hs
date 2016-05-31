{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Safe (headMay)

version = "0.1.0"

type Grade = Int
type Variable = String
type Row = [String]
type Data a = (Variable, a)
data DataType = SingleValue | CrossGrid
type SearchString = String
type NoOfRows = Int
--type SweepSpec = (SearchString, DataType, NoOfRows)
--type ScaleSpec = (DataType, [Variable])
--type SweepSpec2 = (SearchString, ScaleSpec)

data SectionType = Reading | Arithmetic
data Section = Section SectionType Grade
data ScaleSpec = ScaleSpec SearchString DataType [Variable]
data SectionSpec = SectionSpec Section [ScaleSpec]
class ToString a where
    toString :: a -> String

instance ToString (Data String) where
    toString (var, val) = var ++ space var ++ val
instance ToString (Data (Maybe Int)) where
    toString (var, Nothing) = var ++ space var ++ "."
    toString (var, Just x) = var ++ space var ++ show x

space var = replicate (25 - length var) ' '

findTheX :: Row -> Maybe Int
findTheX = fmap fst . headMay . filter (\t -> snd t == "X") . zip [1..]


parseGrid :: Row -> Data (Maybe Int)
parseGrid row = (take 15 (head row), findTheX (tail row))


parseValue :: Row -> Data String
parseValue row = (take 15 (row !! 0), row !! 1)

parseHeader :: [Row] -> [Data String]
parseHeader rows = [
    parseValue (rows !! 0),
    parseValue (rows !! 1),
    parseValue (drop 2 (rows !! 1)),
    parseValue (rows !! 2)
    ]

goto :: SearchString -> [Row] -> Maybe [Row]
goto _ [] = Nothing
goto text (r:rs) = if text `isPrefixOf` head r
                        then Just (r:rs)
                        else goto text rs


toSearchString :: Section -> String
toSearchString (Section sectionType grade) =
    case sectionType of
        Reading ->    "Lesing"
        Arithmetic -> "Regning"
        _ -> "Biff"


isSectionStart :: Row -> Bool
isSectionStart row =
    let cell = head row in
        "Lesing" `isPrefixOf` cell ||
        "Regning" `isPrefixOf` cell ||
        "SOL" `isPrefixOf` cell

extractSection :: Section -> [Row] -> [Row]
extractSection section rows =
    case goto (toSearchString section) of
        Nothing -> []
        Just newRows -> (head rows):(takeWhile isSectionStart (tail rows))

--takeUntilNextSection :: [Row] -> [Row] -> [Row]
--takeUntilNextSection [] result = result
--takeUntilNextSection (r:rs) result =
--    if (isSectionStart r)
--        then result
--        else takeUntilNextSection rs (r:result)



isEmpty :: Row -> Bool
isEmpty = (==) [] . head

sweep :: [Row] -> [SweepSpec] -> ([Row], [String])
sweep rows = foldr hubCore (rows, []) . reverse



jodlCore :: GradeSection -> ([Row], [String]) -> ([Row], [String])
jodlCore (SectionSpec section scaleSpecs) (rows, collectedData) =
    sweep (extractSection section rows)


hubCore :: SweepSpec -> ([Row], [String]) -> ([Row], [String])
hubCore (searchString, dataType, noOfRows) (rows, collectedData) =
    case goto searchString rows of
        Nothing -> (rows, collectedData)
        Just foundRows -> do 
            let parseResult = map (parseVal dataType) (take noOfRows foundRows)
            (foundRows, collectedData ++ parseResult)





parseVal :: DataType -> Row -> String
parseVal dataType =
    case dataType of
        SingleValue -> toString . parseValue
        CrossGrid -> toString . parseGrid

parse :: Handle -> IO [Row]
parse handle = do
    parsed <- parse' []
    return $ reverse parsed
    where
        parse' rows = do
            isEof <- hIsEOF handle
            if isEof
            then
                return rows
            else do
                row <- hGetLine handle
                parse' ((splitOn "," row) : rows)

main :: IO ()
main = do
    fileHandle <- openFile "elevcomma.csv" ReadMode
    rows <- parse fileHandle
    putStrLn $ concat $ head rows
    let cleanRows = filter (not . isEmpty) rows
    let a = (parseHeader . take 3) cleanRows
    let cc = sweep cleanRows spec2008
    let content = map toString a ++ snd cc
    hClose fileHandle
    mapM_ putStrLn content
    let f = foldr (\x y -> x ++ "\n" ++ y) "" content
    withFile "juice.txt" WriteMode (\handle -> hPutStrLn handle f)
 

--spec2008 :: [SweepSpec]
--spec2008 = [
--            ("┼ skrive bokstaver", SingleValue, 6),
--            ("Test not found", CrossGrid, 9),
--            ("Arbeider eleven konsentrert?", CrossGrid, 4),
--            ("┼ lese ord", SingleValue, 4),
--            ("Arbeider eleven konsentrert?", CrossGrid, 4),
--            ("Side 1 - Hvor mange av hver?", SingleValue, 15),
--            ("Side 1 - Sett kryss over like mange", SingleValue, 15),
--            ("1.1 Kjenner igjen", CrossGrid, 4),
--            ("2.1 Har kunnskap om", CrossGrid, 3),
--            ("3.1 Har tilegnet seg", CrossGrid, 5),
--            ("4.1 Har automatisert", CrossGrid, 7),
--            ("\"5.1 Antall parate ordbilder", CrossGrid, 7),
--            ("\"6.1 Har automatisert", CrossGrid, 11),
--            ("7.1 Setningslesingen", CrossGrid, 4),
--            ("\"8.1 Kan bruke ulike begrep", CrossGrid, 12)
--            ]



scalz = [
    SectionSpec (Section Reading 1) [
        ScaleSpec "Særskilt norskopplæring" CrossGrid ["SNO1"],
        ScaleSpec "┼ skrive bokstaver" SingleValue (map ("LN1_" ++) [1..6])
        ]
    SectionSpec (Section Arithmetic 2) [
        ]
    ]
