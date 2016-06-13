{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}  

module SchoolJuice where
    import Data.List (isPrefixOf)
    import qualified Data.Map as Map
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

    data SectionType = Reading | Arithmetic | Sol | Numeracy
    data Section = Section SectionType Grade
    data ScaleSpec = ScaleSpec SearchString DataType [Variable]
    data SectionSpec = SectionSpec Section [ScaleSpec]
    data Cohort = C2006 | C2007 | C2008 deriving (Eq)
    data DataSpec = DataSpec Cohort [SectionSpec]

    class ToString a where
        toString :: a -> String

    instance ToString (Data String) where
        toString (var, val) = var ++ "\t" ++ val
    instance ToString (Data (Maybe Int)) where
        toString (var, Nothing) = var ++ "\t" ++ "."
        toString (var, Just x) = var ++ "\t" ++ show x

    space var = replicate (25 - length var) ' '

    findTheX :: Row -> Maybe Int
    findTheX = fmap fst . headMay . filter (\t -> snd t == "X") . zip [1..]

    parseGrid :: (Variable, Row) -> Data (Maybe Int)
    parseGrid (var, row) = (var, findTheX (tail row))

    parseValue :: (Variable, Row) -> Data String
    parseValue (var, row) = (var, row !! 1)

    parseHeader :: [Row] -> [Data String]
    parseHeader rows = [
        parseValue ("navn", (rows !! 0)),
        parseValue ("hdr1", (rows !! 1)),
        parseValue ("hdr2", (drop 2 (rows !! 1))),
        parseValue ("hdr3", (rows !! 2))
        ]

    goto :: SearchString -> [Row] -> Maybe [Row]
    goto _ [] = Nothing
    goto text (r:rs) = if text `isPrefixOf` head r
                            then Just (r:rs)
                            else goto text rs


    toSearchString :: Cohort -> Section -> String
    toSearchString C2006 (Section Reading 1) = "Leseferdighet 1. trinn (Utgått)"
    toSearchString _ (Section Reading 1) = "Lesing 1.trinn"
    toSearchString _ (Section Reading 2) = "Lesing 2. trinn"
    toSearchString _ (Section Reading 3) = "Lesing 3. trinn"
    toSearchString _ (Section Arithmetic 1) = "Regning 1. trinn"
    toSearchString _ (Section Arithmetic 2) = "Regning 2. trinn"
    toSearchString _ (Section Arithmetic 3) = "Regning 3. trinn"
    toSearchString C2006 (Section Numeracy 1) = "TallforstÕelse og regneferdighet 1.trinn (UtgÕtt)"
    toSearchString cohort (Section Sol grade) = "SOL - " ++ solString
        where solString
                | cohort == C2006 = show (2011 + grade) ++ "/" ++ show (2012 + grade)
                | cohort == C2007 = show (2012 + grade) ++ "/" ++ show (2013 + grade)
                | cohort == C2008 = show (2013 + grade) ++ "/" ++ show (2014 + grade)
    toSearchString _ _ = error "Invalid section."





    isSectionStart :: Row -> Bool
    isSectionStart row =
        let cell = head row in
            "Lesing" `isPrefixOf` cell ||
            "Regning" `isPrefixOf` cell ||
            "SOL" `isPrefixOf` cell

    extractSection :: Cohort -> Section -> [Row] -> [Row]
    extractSection cohort section rows =
        case goto (toSearchString cohort section) rows of
            Nothing -> []
            Just newRows -> (head newRows):(takeWhile (not . isSectionStart) (tail newRows))

    --takeUntilNextSection :: [Row] -> [Row] -> [Row]
    --takeUntilNextSection [] result = result
    --takeUntilNextSection (r:rs) result =
    --    if (isSectionStart r)
    --        then result
    --        else takeUntilNextSection rs (r:result)



    isEmpty :: Row -> Bool
    isEmpty = (==) [] . head

    sweep :: [Row] -> [ScaleSpec] -> ([Row], [String])
    sweep rows = foldr hubCore (rows, []) . reverse


    jodlSweep :: [Row] -> DataSpec -> ([Row], [String])
    jodlSweep rows (DataSpec cohort sectionSpecs) =        
        foldr (jodlCore cohort) (rows, []) (reverse sectionSpecs)


    jodlCore :: Cohort -> SectionSpec -> ([Row], [String]) -> ([Row], [String])
    jodlCore cohort (SectionSpec section scaleSpecs) (rows, collectedData) =
        case goto (toSearchString cohort section) rows of
            Nothing         -> (rows, collectedData)
            Just foundRows  -> do
                let sweepResult = sweep foundRows scaleSpecs
                (fst sweepResult, collectedData ++ (snd sweepResult))
                --sweep (extractSection cohort section rows) scaleSpecs

    --skullCore :: [Row] -> DataSpec -> ([Row], [String])
    --skullCore 


    hubCore :: ScaleSpec -> ([Row], [String]) -> ([Row], [String])
    hubCore (ScaleSpec searchString dataType vars) (rows, collectedData) =
        case goto searchString rows of
            Nothing -> (rows, collectedData)
            Just foundRows -> do 
                let parseResult = map (parseVal dataType) (zip vars foundRows)
                (drop (length vars) foundRows, collectedData ++ parseResult)


    createVariables :: String -> [Int] -> [String]
    createVariables varBase = map ((++) varBase . show)


    parseVal :: DataType -> (Variable, Row) -> String
    parseVal dataType =
        case dataType of
            SingleValue -> toString . parseValue
            CrossGrid -> toString . parseGrid

