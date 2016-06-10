{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}  

module Main where
    import System.IO
    import Data.List.Split (splitOn)
    import SchoolJuice

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




    --data SectionType = Reading | Arithmetic
    --data Section = Section SectionType Grade
    --data ScaleSpec = ScaleSpec SearchString DataType [Variable]
    --data SectionSpec = SectionSpec Section [ScaleSpec]