import Test.Hspec
import SchoolJuice
import qualified Data.Map as Map

main = hspec $ do
    describe "Test configuration" $ do
        it "is correctly set up" $ do
            True `shouldBe` True

    describe "findTheX" $ do
        it "returns Nothing when no x is found" $ do
            findTheX [] `shouldBe` Nothing

        it "returns 1 when x is in first column" $ do
            findTheX ["X", "", "", ""] `shouldBe` Just 1

        it "returns 4 when x is in fourth column" $ do
            findTheX ["", "", "", "X"] `shouldBe` Just 4

    --describe "parseValue" $ do
    --    it "returns empty when input is empty" $ do
    --        parseValue 

    describe "dropMiddleNames" $ do
        context "when full name has two names" $ do
            it "drops nothing" $ do
                dropMiddleNames "Harry Potter" `shouldBe` "Harry Potter"
        context "when full name has five names" $ do
            it "drops middle names" $
                dropMiddleNames "Albus Percival Wulfric Brian Dumbledore" `shouldBe` "Albus Dumbledore"




    let testRows = [
            ["test1", "1"],
            ["test2", "2"],
            ["test3", "3"],
            ["grid1", "X", "", ""],
            ["grid2", "", "X", ""],
            ["grid3", "", "", "X"],
            ["abc1", "11"],
            ["abc2", "22"],
            ["abc3", "33"]
            ]

    let testRowsProcessed = Map.fromList [("t1", "1"), ("t2", "2"), ("t3", "3"), ("g1", "1"), ("g2", "2"), ("g3", "3"), ("a1", "11"), ("a2", "22"), ("a3", "33")]

    describe "hubCore" $ do
        let singleTestRow = ([["testvar", "testval"]])


        it "doesn't produce new data when searching for nonexistent text." $ do
            let spec = ScaleSpec "Nonexistent text" SingleValue ["testvar"]
            hubCore spec (testRows, Map.empty) `shouldBe` (testRows, Map.empty)

        it "doesn't produce new data without variables." $ do
            let spec = ScaleSpec "testvar" SingleValue []
            hubCore spec (singleTestRow, Map.empty) `shouldBe` (singleTestRow, Map.empty)

        it "returns new data when 1) search string exists, 2) one variable provided" $ do
            let spec = ScaleSpec "testvar" SingleValue ["tv01"]
            hubCore spec (singleTestRow, Map.empty) `shouldBe`
                ([], Map.fromList [("tv01", "testval")])

        it "returns new data when 1) search string exists, 2) found data is in bottom of dataset, 3) multiple variables provided" $ do
            let spec = ScaleSpec "abc1" SingleValue ["a", "b", "c"]
            hubCore spec (testRows, Map.empty) `shouldBe`
                ([], Map.fromList [("a", "11"), ("b", "22"), ("c", "33")])

        it "appends new data to input data" $ do
            let spec = ScaleSpec "testvar" SingleValue ["tv01"]
            hubCore spec (singleTestRow, Map.fromList [("existing", "data")]) `shouldBe`
                ([], Map.fromList [("existing", "data"), ("tv01", "testval")])

        it "drops processed rows" $ do
            let spec = ScaleSpec "test1" SingleValue ["t1", "t2", "t3"]
            hubCore spec (testRows, Map.empty) `shouldBe`
                ((drop 3 testRows), Map.fromList [("t1", "1"), ("t2", "2"), ("t3", "3")])

    describe "sweep" $ do
        it "sweeps the data." $ do
            let specs = [
                    ScaleSpec "test1" SingleValue ["t1", "t2", "t3"],
                    ScaleSpec "grid1" CrossGrid ["g1", "g2", "g3"],
                    ScaleSpec "abc1" SingleValue ["a1", "a2", "a3"]
                    ]
            sweep testRows specs `shouldBe`
                ([], testRowsProcessed)
        --ScaleSpec SearchString DataType [Variable]


    describe "toSearchString" $ do
        it "creates correct search string for 2nd grade" $ do
            toSearchString C2008 (Section Reading 2) `shouldBe` "Lesing 2. trinn"

    --describe "extractSection" $ do
    --    it "returns empty list when input is empty" $ do
    --        extractSection C2008 (Section Reading 1) [] `shouldBe` []

    --    it "finds a section" $ do
    --        let testRows = [["Lesing 2. trinn"], ["Some", "data"]]
    --        extractSection C2008 (Section Reading 2) testRows `shouldBe` 
    --            [["Lesing 2. trinn"], ["Some", "data"]]

    --    it "finds the section in the middle" $ do
    --        let testRows = [
    --                ["Lesing 1.trinn"],
    --                ["Random", "data"],
    --                ["More", "data"],
    --                ["Lesing 2. trinn"],
    --                ["testvar", "testval"],
    --                ["testvar2", "testval2"],
    --                ["Lesing 3. trinn"],
    --                ["Random", "data"]
    --                ]
    --        extractSection C2008 (Section Reading 2) testRows `shouldBe` 
    --            [["Lesing 2. trinn"], ["testvar", "testval"], ["testvar2", "testval2"]]


    --describe "jodlCore" $ do
    --    it "jodls." $ do
    --        let testSection = ["Lesing 1.trinn - Utdanningsdirektoratet - Vår 2013 - 23.04.2013"]:testRows
    --        let scaleSpecs = [
    --                ScaleSpec "test1" SingleValue ["t1", "t2", "t3"],
    --                ScaleSpec "grid1" CrossGrid ["g1", "g2", "g3"],
    --                ScaleSpec "abc1" SingleValue ["a1", "a2", "a3"]
    --                ]
    --        let sectionSpec = SectionSpec (Section Reading 1) scaleSpecs

    --        jodlCore C2008 sectionSpec (testSection, Map.empty) `shouldBe`
    --            ([], testRowsProcessed)

    --    it "finds the data" $ do
    --        let testRows = [["Lesing 1.trinn"], ["Random", "data"], ["Lesing 2. trinn"], ["testvar", "testval"]]
    --        let scaleSpec = ScaleSpec "testvar" SingleValue ["t1"]
    --        let sectionSpec = SectionSpec (Section Reading 2) [scaleSpec]

    --        jodlCore C2008 sectionSpec (testRows, Map.empty) `shouldBe`
    --            ([], Map.fromList [("t1", "testval")])



    --describe "jodlSweep" $ do        
    --    it "works for one section" $ do
    --        let testRows = [["Lesing 1.trinn"], ["testvar", "testval"]]

    --        let sectionSpec = SectionSpec (Section Reading 1) [ScaleSpec "testvar" SingleValue ["t1"]]
    --        jodlSweep testRows (DataSpec C2008 [sectionSpec]) `shouldBe`
    --            ([], Map.fromList [("t1", "testval")])

    --    it "works for two sections" $ do
    --        let testRows = [
    --                        ["Lesing 1.trinn"], ["testvar", "testval"],
    --                        ["Lesing 2. trinn"], ["testvar2", "testval2"]
    --                        ]
    --        let sectionSpecs = [
    --                SectionSpec (Section Reading 1) [ScaleSpec "testvar" SingleValue ["t1"]],
    --                SectionSpec (Section Reading 2) [ScaleSpec "testvar2" SingleValue ["t2"]]
    --                ]
    --        jodlSweep testRows (DataSpec C2008 sectionSpecs) `shouldBe`
    --            ([], Map.fromList [("t1", "testval"), ("t2", "testval2")])



    --    it "jodlsweeps." $ do
    --        let testRows = [
    --                        ["Lesing 1.trinn - Utdanningsdirektoratet - 2014/2015 - 28.04.2015"],
    --                        ["L1", "1"],
    --                        ["Lesing 2. trinn - Utdanningsdirektoratet - 2013/2014 - 06.05.2014"],
    --                        ["L2", "2"],
    --                        ["Lesing 3. trinn - Utdanningsdirektoratet - 2015/2016 - 13.04.2016"],
    --                        ["L3", "3"],
    --                        ["Regning 1.trinn - Utdanningsdirektoratet - 2013/2014 - 13.05.2014"],
    --                        ["R1", "1"],
    --                        ["Regning 2. trinn - Utdanningsdirektoratet - 2014/2015 - 20.04.2015"],
    --                        ["R2", "2"],
    --                        ["Regning 3. trinn - Utdanningsdirektoratet - 2015/2016 - 24.04.2016"],
    --                        ["R3", "3"],
    --                        ["SOL - 2015/2016 - 01.12.2015"],
    --                        ["S3", "3"],
    --                        ["SOL - 2014/2015 - 11.03.2015"],
    --                        ["S2", "2"],
    --                        ["SOL - 2013/2014 - 12.05.2014"],
    --                        ["S1", "1"]
    --                        ]
    --        let sectionSpecs = [
    --                SectionSpec (Section Reading 1) [ScaleSpec "L1" SingleValue ["L1"]],
    --                SectionSpec (Section Reading 2) [ScaleSpec "L2" SingleValue ["L2"]],
    --                SectionSpec (Section Reading 3) [ScaleSpec "L3" SingleValue ["L3"]],
    --                SectionSpec (Section Arithmetic 1) [ScaleSpec "R1" SingleValue ["R1"]],
    --                SectionSpec (Section Arithmetic 2) [ScaleSpec "R2" SingleValue ["R2"]],
    --                SectionSpec (Section Arithmetic 3) [ScaleSpec "R3" SingleValue ["R3"]],
    --                SectionSpec (Section Sol 3) [ScaleSpec "S3" SingleValue ["S3"]],
    --                SectionSpec (Section Sol 2) [ScaleSpec "S2" SingleValue ["S2"]],
    --                SectionSpec (Section Sol 1) [ScaleSpec "S1" SingleValue ["S1"]]
    --                ]

    --        jodlSweep testRows (DataSpec C2006 sectionSpecs) `shouldBe`
    --            ([], Map.fromList [("L1", "1"), ("L2", "2"), ("L3", "3"), ("R1", "1"), ("R2", "2"), ("R3", "3"), ("S3", "3"), ("S2", "2"), ("S1", "1")])

        --it ("does not go to wrong section.") $ do
        --    let testRows = [
        --                    ["Lesing 1.trinn - Utdanningsdirektoratet - 2013/2014 - 06.05.2014"],
        --                    ["Lesing 2. trinn - Utdanningsdirektoratet - 2013/2014 - 06.05.2014"],
        --                    ["testvar", "testval"]
        --                ]

        --    let nonexistentSection = SectionSpec (Section Reading 1) [ScaleSpec "testvar" SingleValue ["g1_t1"]]            
        --    let sectionSpecs = [
        --            nonexistentSection,
        --            SectionSpec (Section Reading 2) [ScaleSpec "testvar" SingleValue ["g2_t1"]]
        --            ]

        --    jodlSweep testRows (DataSpec C2008 sectionSpecs) `shouldBe`
        --        ([], ["g2_t1\ttestval", "a"])
