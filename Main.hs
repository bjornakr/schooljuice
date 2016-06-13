{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}  

module Main where
    import System.FilePath (takeExtension)
    import System.Directory (getDirectoryContents)
    import System.IO
    import qualified Data.Map as Map
    import Data.List.Split (splitOn)
    import SchoolJuice

    --isCsvFile :: FilePath
    --isCsvFile file = 

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


    main :: IO ()
    main = do
        allFiles <- getDirectoryContents "."
        let csvFiles = filter (\x -> takeExtension x == ".csv") allFiles
        print csvFiles

    main2 :: IO ()
    main2 = do
        fileHandle <- openFile "2007-2.csv" ReadMode
        rows <- parse fileHandle
        putStrLn $ concat $ head rows
        let cleanRows = filter (not . isEmpty) rows
        let a = (parseHeader . take 3) cleanRows
        let cc = jodlSweep cleanRows spec2007
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



    spec2006 = DataSpec C2006 [
        SectionSpec (Section Reading 1) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_1"],
            ScaleSpec "Del 2 - ┼ skrive bokstavene" SingleValue $ createVariables "L1_" [1..8]
            ],
        SectionSpec (Section Reading 2) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_2"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L2_" [1..4]
            ],
        SectionSpec (Section Reading 3) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_3"],
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
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_1"],
            ScaleSpec "┼ skrive bokstaver" SingleValue $ createVariables "L1_" [1..6] -- (map ((++) "LN1_" . show) [1..6])            
            ],
        SectionSpec (Section Reading 2) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_2"],
            ScaleSpec "┼ lese ord" SingleValue $ createVariables "L2_" [1..4]
            ],        
        SectionSpec (Section Reading 3) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_3"],
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
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_1"],
            ScaleSpec "┼ skrive bokstaver" SingleValue $ createVariables "L1_" [1..6] -- (map ((++) "LN1_" . show) [1..6])            
            ],
        SectionSpec (Section Reading 2) [
            ScaleSpec "Sµrskilt norskopplµring" CrossGrid ["SNO_2"],
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

    vars = ["Elev", "Kjønn", "Skole1", "Skole2", "Skole3", "SNO1", "SNO2", "SNO3",
            
            "TestinfoL1",
            "L1_2", "L1_3", "L1_4", "L1_5", "L1_6", "L1_7", "L1_8",
            
            "TestinfoL2",
            "L2_1", "L2_2", "L2_3", "L2_4",
            
            "TestinfoL3",
            "L3_1", "L3_2", "L3_3", "L3_4",
            
            "TestinfoR2",
            "R2_1", "R2_2", "R2_3", "R2_4", "R2_5", "R2_6", "R2_7", "R2_8",
            "R2_10", "R2_11", "R2_12", "R2_13", "R2_14", "R2_15", "R2_16",
            
            "TestinfoR3",
            "R3_1", "R3_2", "R3_3", "R3_4", "R3_5", "R3_6", "R3_7", "R3_8",
            "R3_9", "R3_10", "R3_13", "R3_14", "R3_15", "R3_16", "R3_17", "R3_18", "R3_19", "R3_20",
            
            "TestinfoS1", "S1_11", "S1_12", "S1_13", "S1_14", "S1_21", "S1_22", "S1_23",
            "S1_31", "S1_32", "S1_33", "S1_34", "S1_35",
            "S1_41", "S1_42", "S1_43", "S1_44", "S1_45", "S1_46", "S1_47",
            "S1_51", "S1_52", "S1_53", "S1_54", "S1_55", "S1_56", "S1_57",
            
            "TestinfoS2", "S2_11", "S2_12", "S2_13", "S2_14", "S2_21", "S2_22", "S2_23",
            "S2_31", "S2_32", "S2_33", "S2_34", "S2_35",
            "S2_41", "S2_42", "S2_43", "S2_44", "S2_45", "S2_46", "S2_47",
            "S2_51", "S2_52", "S2_53", "S2_54", "S2_55", "S2_56", "S2_57",
            "S2_61", "S2_62", "S2_63", "S2_64", "S2_65", "S2_66", "S2_67", "S2_68", "S2_69", "S2_610", "S2_611",
            
            "TestinfoS3", "S3_11", "S3_12", "S3_13", "S3_14", "S3_21", "S3_22", "S3_23",
            "S3_31", "S3_32", "S3_33", "S3_34", "S3_35",
            "S3_41", "S3_42", "S3_43", "S3_44", "S3_45", "S3_46", "S3_47",
            "S3_51", "S3_52", "S3_53", "S3_54", "S3_55", "S3_56", "S3_57",
            "S3_61", "S3_62", "S3_63", "S3_64", "S3_65", "S3_66", "S3_67", "S3_68", "S3_69", "S3_610", "S3_611",

            "TestinfoTR1", "TR1_1", "TR1_2", "TR1_3", "TR1_4", "TR1_5", "TR1_6", "TR1_7", "TR1_8",
            "TR1_9", "TR1_10", "TR1_11"
            ]


    --data SectionType = Reading | Arithmetic
    --data Section = Section SectionType Grade
    --data ScaleSpec = ScaleSpec SearchString DataType [Variable]
    --data SectionSpec = SectionSpec Section [ScaleSpec]