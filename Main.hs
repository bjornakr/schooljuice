{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}  

module Main where
    import System.FilePath (takeExtension)
    import System.Directory (getDirectoryContents, makeAbsolute)
    import System.IO
    import qualified Data.Map as Map
    import Data.List (intercalate)
    import Data.List.Split (splitOn)
    import SchoolJuice

    dataPath = "./csv/"
    outfile = "juice.csv"
    --isCsvFile :: FilePath
    --isCsvFile file = 
    getSpec :: Maybe String -> DataSpec
    getSpec Nothing = error "Missing birthyear."
    getSpec (Just birthYear) = case (drop 6 birthYear) of
        "2006" -> spec2006
        "2007" -> spec2007
        "2008" -> spec2008
        _ -> error $ "Invalid birthyear: " ++ birthYear

    parse :: Handle -> IO [Row]
    parse handle = do
        parsed <- parse' []
        return $ reverse parsed
        where
            parse' rows = do
                isEof <- hIsEOF handle
                if isEof
                    then return rows
                    else do
                        row <- hGetLine handle
                        parse' ((splitOn ";" row) : rows)



    dataToCsv :: Map.Map String String -> String
    dataToCsv m = cc $ map (\var -> Map.lookup var m) vars
        where
            cc :: [Maybe String] -> String
            cc [] = ""
            cc (Nothing:xs) = ";" ++ (cc xs)
            cc ((Just x):xs) = x ++ ";" ++ (cc xs)


    main2 :: IO ()
    main2 = do
        allFiles <- getDirectoryContents "."
        let csvFiles = filter (\x -> takeExtension x == ".csv") allFiles
        print csvFiles


    createNameIdMap :: IO (Map.Map String String)
    createNameIdMap = do
        idFile <- openFile "ids.csv" ReadMode
        rows <- parse idFile
        return $ Map.fromList (map (\row -> (dropMiddleNames(name row) ++ (birthDate row), id0 row)) rows)
        where 
            id0 row = row !! 0
            name row = (row !! 1) ++ " " ++ (row !! 2)
            birthDate row = row !! 3

    addIdToHeader :: Map.Map String String -> Map.Map String String -> Map.Map String String
    addIdToHeader header nameIdMap =
        let nameMay = dropMiddleNames <$> (Map.lookup "navn" header)
            birthDateMay = Map.lookup "fdato" header
            keyMay = (++) <$> nameMay <*> birthDateMay
            idMay = keyMay >>= ((flip Map.lookup) nameIdMap) 
        in case idMay of
            (Just id0)  -> Map.insert "id" id0 header
            (Nothing)   -> header

    processFile :: FilePath -> IO (String)
    processFile filePath = do        
        fileHandle <- openFile filePath ReadMode
        rows <- parse fileHandle
        putStrLn $ concat $ head rows
        let cleanRows = filter (not . isEmpty) rows
        let header = (parseHeader . take 3) cleanRows
        let rest = snd $ jodlSweep cleanRows (getSpec (Map.lookup "fdato" header))

        nameIdMap <- createNameIdMap
        --let idMay =
        --        case (Map.lookup "navn" header, Map.lookup "fdato" header) of
        --            (Just navn, Just birthDate) -> 
        --                Map.lookup ((dropMiddleNames navn) ++ birthDate) nameIdMap
        --            _ -> Nothing

        --let headerWithId =
        --        case idMay of
        --            (Just id0)  -> Map.insert "id" id0 header
        --            Nothing     -> header
         

        --let content = map (\(x, y) -> x ++ "\t\t" ++ y) (Map.toList (Map.union header rest))
        --mapM_ putStrLn content
        hClose fileHandle

        let headerWithId = addIdToHeader header nameIdMap

        return $ dataToCsv (Map.union headerWithId rest)


    main :: IO ()
    main = do
        allFiles <- getDirectoryContents dataPath
        let csvFiles = filter (\x -> takeExtension x == ".csv" && x /= outfile && x /= "ids.csv") allFiles
        --let absPath = map makeAbsolute csvFiles
        allData <- mapM (\x -> processFile (dataPath ++ x)) csvFiles



        --fileHandle <- openFile "2007-2.csv" ReadMode
        --rows <- parse fileHandle
        --putStrLn $ concat $ head rows
        --let cleanRows = filter (not . isEmpty) rows
        --let header = (parseHeader . take 3) cleanRows
        --let scales = snd $ jodlSweep cleanRows spec2007
        --let content = map (\(x, y) -> x ++ "\t\t" ++ y) (Map.toList (Map.union header scales))
        --hClose fileHandle
        --mapM_ putStrLn content
        --let f = foldr (\x y -> x ++ "\n" ++ y) "" content
        withFile "juice.csv" WriteMode (\handle -> do
            hPutStrLn handle (intercalate ";" vars)
            
            mapM_ (hPutStrLn handle) allData
            )
     



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



    spec2006 = DataSpec C2006 [
        SectionSpec (Section Reading 1) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO1"],
            ScaleSpec "Del 2 - ┼ skrive bokstavene" SingleValue $ createVariables "L1_" [1..8]
            ],
        SectionSpec (Section Reading 2) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO2"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L2_" [1..4]
            ],
        SectionSpec (Section Reading 3) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO3"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L3_" [1..4]
            ],
        SectionSpec (Section Arithmetic 2) [
            ScaleSpec "Side 1 - Hvor mange av hver?" SingleValue $ createVariables "R2_" [1..15]
            ],
        SectionSpec (Section Arithmetic 3) [
            ScaleSpec "Side 1 - Hvor mye?" SingleValue $ createVariables "R3_" [1..18]
            ],
        SectionSpec (Section Sol 3) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S3_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S3_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S3_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S3_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S3_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S3_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S3_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S3_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S3_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S3_10_" [1..4]
            ],
        SectionSpec (Section Sol 2) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S2_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S2_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S2_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S2_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S2_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S2_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S2_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S2_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S2_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S2_10_" [1..4]
            ],
        SectionSpec (Section Sol 1) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S1_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S1_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S1_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S1_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S1_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S1_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S1_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S1_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S1_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S1_10_" [1..4]
            ],
        SectionSpec (Section Numeracy 1) [
            ScaleSpec "Side 1" SingleValue $ createVariables "TR1_" [1..11]
            ]
        ]



    spec2007 = DataSpec C2007 [
        SectionSpec (Section Reading 1) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO1"],
            ScaleSpec "┼ skrive bokstaver" SingleValue $ createVariables "L1_" [1..6] -- (map ((++) "LN1_" . show) [1..6])            
            ],
        SectionSpec (Section Reading 2) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO2"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L2_" [1..4]
            ],        
        SectionSpec (Section Reading 3) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO3"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L3_" [1..4]
            ],
        SectionSpec (Section Arithmetic 1) [
            ScaleSpec "Side 1 - Sett kryss over like mange" SingleValue $ createVariables "R1_" [1..15]
            ],
        SectionSpec (Section Arithmetic 2) [
            ScaleSpec "Side 1 - Hvor mange av hver?" SingleValue $ createVariables "R2_" [1..15]
            ],
        SectionSpec (Section Arithmetic 3) [
            ScaleSpec "Side 1 - Hvor mye?" SingleValue $ createVariables "R3_" [1..18]
            ],
        SectionSpec (Section Sol 3) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S3_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S3_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S3_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S3_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S3_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S3_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S3_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S3_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S3_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S3_10_" [1..4]
            ],
        SectionSpec (Section Sol 2) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S2_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S2_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S2_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S2_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S2_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S2_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S2_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S2_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S2_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S2_10_" [1..4]
            ],
        SectionSpec (Section Sol 1) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S1_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S1_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S1_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S1_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S1_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S1_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S1_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S1_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S1_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S1_10_" [1..4]
            ]
        ]


    spec2008 = DataSpec C2008 [
        SectionSpec (Section Reading 1) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO1"],
            ScaleSpec "┼ skrive bokstaver" SingleValue $ createVariables "L1_" [1..6] -- (map ((++) "LN1_" . show) [1..6])            
            ],
        SectionSpec (Section Reading 2) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO2"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L2_" [1..4]
            ],
        SectionSpec (Section Arithmetic 1) [
            ScaleSpec "Side 1 - Sett kryss over like mange" SingleValue $ createVariables "R1_" [1..15]
            ],
        SectionSpec (Section Arithmetic 2) [
            ScaleSpec "Side 1 - Hvor mange av hver?" SingleValue $ createVariables "R2_" [1..15]
            ],
        SectionSpec (Section Sol 2) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S2_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S2_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S2_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S2_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S2_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S2_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S2_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S2_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S2_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S2_10_" [1..4]
            ],
        SectionSpec (Section Sol 1) [
            ScaleSpec "1.1 Kjenner igjen navnet sitt." CrossGrid $ createVariables "S1_1_" [1..4],
            ScaleSpec "2.1 Har kunnskap om noen bokstaver." CrossGrid $ createVariables "S1_2_" [1..3],
            ScaleSpec "3.1 Har tilegnet seg det alfabetiske prinsipp." CrossGrid $ createVariables "S1_3_" [1..5],
            ScaleSpec "4.1 Har automatisert de enkle bokstav/lyd" CrossGrid $ createVariables "S1_4_" [1..7],
            ScaleSpec "5.1 Antall parate ordbilder °ker." CrossGrid $ createVariables "S1_5_" [1..7],
            ScaleSpec "6.1 Har automatisert de komplekse grafemene" CrossGrid $ createVariables "S1_6_" [1..11],
            ScaleSpec "7.1 Setningslesingen er automatisert." CrossGrid $ createVariables "S1_7_" [1..4],
            ScaleSpec "8.1 Kan bruke ulike begrep til Õ samtale" CrossGrid $ createVariables "S1_8_" [1..12],
            ScaleSpec "9.1 Bruker og velger rett strategi" CrossGrid $ createVariables "S1_9_" [1..5],
            ScaleSpec "10.1 Leser med innsikt" CrossGrid $ createVariables "S1_10_" [1..4]
            ]
        ]

    vars = ["navn","id", "fdato", "gruppe", "kjonn", "skole1", "skole2", "skole3", "SNO1", "SNO2", "SNO3",
            
            "testdatoL1",
            "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L1_7", "L1_8",
            
            "testdatoL2",
            "L2_1", "L2_2", "L2_3", "L2_4",
            
            "testdatoL3",
            "L3_1", "L3_2", "L3_3", "L3_4",
            
            "testdatoR2",
            "R2_1", "R2_2", "R2_3", "R2_4", "R2_5", "R2_6", "R2_7", "R2_8",
            "R2_10", "R2_11", "R2_12", "R2_13", "R2_14", "R2_15", "R2_16",
            
            "testdatoR3",
            "R3_1", "R3_2", "R3_3", "R3_4", "R3_5", "R3_6", "R3_7", "R3_8",
            "R3_9", "R3_10", "R3_13", "R3_14", "R3_15", "R3_16", "R3_17", "R3_18", "R3_19", "R3_20",
            
            "testdatoS1", "S1_1_1", "S1_1_2", "S1_1_3", "S1_1_4", "S1_2_1", "S1_2_2", "S1_2_3",
            "S1_3_1", "S1_3_2", "S1_3_3", "S1_3_4", "S1_3_5",
            "S1_4_1", "S1_4_2", "S1_4_3", "S1_4_4", "S1_4_5", "S1_4_6", "S1_4_7",
            "S1_5_1", "S1_5_2", "S1_5_3", "S1_5_4", "S1_5_5", "S1_5_6", "S1_5_7",
            
            "testdatoS2", "S2_1_1", "S2_1_2", "S2_1_3", "S2_1_4", "S2_2_1", "S2_2_2", "S2_2_3",
            "S2_3_1", "S2_3_2", "S2_3_3", "S2_3_4", "S2_3_5",
            "S2_4_1", "S2_4_2", "S2_4_3", "S2_4_4", "S2_4_5", "S2_4_6", "S2_4_7",
            "S2_5_1", "S2_5_2", "S2_5_3", "S2_5_4", "S2_5_5", "S2_5_6", "S2_5_7",
            "S2_6_1", "S2_6_2", "S2_6_3", "S2_6_4", "S2_6_5", "S2_6_6", "S2_6_7",
            "S2_6_8", "S2_6_9", "S2_6_10", "S2_6_11",
            
            "testdatoS3", "S3_1_1", "S3_1_2", "S3_1_3", "S3_1_4", "S3_2_1", "S3_2_2", "S3_2_3",
            "S3_3_1", "S3_3_2", "S3_3_3", "S3_3_4", "S3_3_5",
            "S3_4_1", "S3_4_2", "S3_4_3", "S3_4_4", "S3_4_5", "S3_4_6", "S3_4_7",
            "S3_5_1", "S3_5_2", "S3_5_3", "S3_5_4", "S3_5_5", "S3_5_6", "S3_5_7",
            "S3_6_1", "S3_6_2", "S3_6_3", "S3_6_4", "S3_6_5", "S3_6_6", "S3_6_7",
            "S3_6_8", "S3_6_9", "S3_6_10", "S3_6_11",

            "testdatoTR1", "TR1_1", "TR1_2", "TR1_3", "TR1_4", "TR1_5", "TR1_6", "TR1_7", "TR1_8",
            "TR1_9", "TR1_10", "TR1_11"
            ]


    --data SectionType = Reading | Arithmetic
    --data Section = Section SectionType Grade
    --data ScaleSpec = ScaleSpec SearchString DataType [Variable]
    --data SectionSpec = SectionSpec Section [ScaleSpec]