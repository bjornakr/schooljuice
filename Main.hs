{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

import System.IO
import Data.List (isPrefixOf)
import Data.List.Split (splitOn)
import Safe (headMay)
--import qualified Data.Text as T

version = "0.1.0"


type Variable = String
type Row = [String]
type Data a = (Variable, a)
data DataType = Value | CrossGrid

type SweepSpec = (String, DataType, Int)

class ToString a where
    toString :: a -> String

instance ToString (Data String) where
    toString (var, val) = var ++ (space var) ++ val
instance ToString (Data (Maybe Int)) where
    toString (var, Nothing) = var ++ (space var) ++ "."
    toString (var, Just x) = var ++ space var ++ show x

space var = replicate (25-(length var)) ' '

findTheX :: Row -> Maybe Int
findTheX = fmap fst . headMay . filter (\t -> snd t == "X") . zip [1..]


parseGrid :: Row -> Data (Maybe Int)
parseGrid row = (take 15 (head row), findTheX (tail row))


parseValue :: Row -> Data String
parseValue r = (take 15 (r !! 0), r !! 1)

parseHeader :: [Row] -> [Data String]
parseHeader rows = [
    parseValue (rows !! 0),
    parseValue (rows !! 1),
    parseValue (drop 2 (rows !! 1)),
    parseValue (rows !! 2)
    ]


goto :: String -> [Row] -> Maybe [Row]
goto _ [] = Nothing
goto text (r:rs) = if text `isPrefixOf` (head r)
                        then Just (r:rs)
                        else goto text rs




splitRow :: String -> [String]
splitRow = splitOn ","

parse handle = do
    a <- parse' []
    return $ reverse a
    where
        parse' rows = do
            isEof <- hIsEOF handle
            if isEof
            then
                return rows
            else do
                row <- hGetLine handle
                parse' (splitRow row : rows)


isEmpty :: Row -> Bool
isEmpty r = r !! 0 == [] -- || (length r >= 2 && r !! 1 == []) -- [] == filter (/= "") r

--showData :: Data String -> String
--showData (var, val) = var ++ space var ++ val

--showDatax :: Data (Maybe Int) -> String
--showDatax (var, Nothing) = var ++ space var ++ ""
--showDatax (var, Just x) = var ++ space var ++ show x

--takeMay :: Int -> Maybe [a] -> Maybe [a]
--takeMay = (fmap . take)

--next old Nothing = old
--next old (Just new) = new


--beefCore :: (Row -> Data a) -> Maybe [Row] -> [Data a]
--beefCore _ Nothing = []
--beefCore f (Just rows) = map f rows

main = do
    fileHandle <- openFile "elevcomma.csv" ReadMode
    rows <- parse fileHandle
    putStrLn $ concat $ rows !! 0
    let cleanRows = filter (not . isEmpty) rows
    --let z = map processGrid cleanRows
    let a = (parseHeader . take 3) cleanRows
    --let cleanRows1 = goto "┼ skrive bokstaver" cleanRows

    --let cleanRows1 = goto "Arbeider eleven konsentrert?" cleanRows



    ----let b = map parseData (take 6 (next cleanRows cleanRows1))
    --let b = beefCore parseData (takeMay 6 cleanRows1)
    --let cleanRows2 = goto "1.1 Kjenner igjen navnet sitt." cleanRows
    --let c = beefCore processGrid (takeMay 4 cleanRows2)
    --let content = (map toString a ++ map toString b ++ map toString c)

    let cc = sweep cleanRows dosMilOcho

    let content = snd cc

    mapM putStrLn content
    --mapM (putStrLn . toString) $ b ++ b ++ c

    --let content = map toString (a ++ b ++ c)
    let f = foldr (\x y -> x ++ "\n" ++ y) "" content

    withFile "juice.txt" WriteMode (\handle -> do hPutStrLn handle f)
 

dosMilOcho :: [(SweepSpec)]
dosMilOcho = [
                ("┼ skrive bokstaver", Value, 6),
                ("Test not found", CrossGrid, 9),
                ("Arbeider eleven konsentrert?", CrossGrid, 4),
                ("┼ lese ord", Value, 4),
                ("Arbeider eleven konsentrert?", CrossGrid, 4),
                ("Side 1 - Hvor mange av hver?", Value, 15),
                ("Side 1 - Sett kryss over like mange", Value, 15),
                ("1.1 Kjenner igjen", CrossGrid, 4),
                ("2.1 Har kunnskap om", CrossGrid, 3),
                ("3.1 Har tilegnet seg", CrossGrid, 5),
                ("4.1 Har automatisert", CrossGrid, 7),
                ("\"5.1 Antall parate ordbilder", CrossGrid, 7),
                ("\"6.1 Har automatisert", CrossGrid, 11),
                ("7.1 Setningslesingen", CrossGrid, 4),
                ("\"8.1 Kan bruke ulike begrep", CrossGrid, 12)
                ]



sweep :: [Row] -> [SweepSpec] -> ([Row], [String])
sweep rows = foldr hubCore (rows, []) . reverse

hubCore :: SweepSpec -> ([Row], [String]) -> ([Row], [String])
hubCore (searchString, dataType, noOfRows) (rows, datas) =
    case goto searchString rows of
        Nothing -> (rows, datas)
        Just found -> do 
            let bugg = map (parseVal dataType) (take noOfRows found)
            (found, datas ++ bugg)


parseVal :: DataType -> Row -> String
parseVal dataType =
    case dataType of
        Value -> toString . parseValue
        CrossGrid -> toString . parseGrid


-- do
    -- process "Searchstring" < Value | CrossGrid > <# of lines>
    -- process "Searchstring" < Value | CrossGrid > <# of lines>
    -- process "Searchstring" < Value | CrossGrid > <# of lines>
