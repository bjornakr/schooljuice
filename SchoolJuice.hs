{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}  

module SchoolJuice where
    import Data.List (isPrefixOf)
    import Data.List.Split (splitOn)
    import qualified Data.Map as Map
    import Safe (headMay)
    import Data.Maybe (fromMaybe)

    version = "0.1.0"

    type Grade = Int
    type Variable = String
    type Row = [String]
    data DataMap = Map String String
    data DataType = SingleValue | CrossGrid
    type SearchString = String
    type NoOfRows = Int

    data SectionType = Reading | Arithmetic | Sol | Numeracy
    data Section = Section SectionType Grade
    data ScaleSpec = ScaleSpec SearchString DataType [Variable]
    data SectionSpec = SectionSpec Section [ScaleSpec]
    data Cohort = C2006 | C2007 | C2008 deriving (Eq)
    data DataSpec = DataSpec Cohort [SectionSpec]

    class ToString a where
        toString :: a -> String


    findTheX :: Row -> Maybe Int
    findTheX = fmap fst . headMay . filter (\t -> snd t == "X") . zip [1..]

    parseGrid :: (Variable, Row) -> Maybe (Variable, String)
    parseGrid (var, row) = 
        case findTheX (tail row) of
            Just x ->  Just (var, show x)
            Nothing -> Nothing

    parseValue :: (Variable, Row) -> (Variable, String)
    parseValue (var, row) = (var, row !! 1)

    parseHeader :: [Row] -> Map.Map String String
    parseHeader rows = Map.fromList [
        parseValue ("navn", rows !! 0),
        parseValue ("fdato", rows !! 1),
        (fst gruppe, "'" ++ snd gruppe ++ "'"), -- wrapping in quotes to prevent excel converting it to date
        parseValue ("kjonn", rows !! 2)
        ]
        where gruppe = parseValue ("gruppe", drop 2 (rows !! 1))

    insertBirthDateIfMissing :: Map.Map String String -> Map.Map String String
    insertBirthDateIfMissing header =
        case (Map.lookup "fdato" header) of
            Just [] ->
                let bd = do
                        name <- Map.lookup "navn" header
                        birthdate <- Map.lookup name birthdates
                        return birthdate
                in Map.insert "fdato" (fromMaybe "EMPTY" bd) header

            --Map.insert "fdato" (fromMaybe "" (Map.lookup birthdates (fromMaybe "" (Map.lookup "name")))) header 
            _ -> header

    parseTestDate :: Section -> Row -> Map.Map String String
    parseTestDate (Section sectionType grade) row =
        Map.singleton 
            ("testdato" ++ sectionLetter sectionType ++ show grade)
            (drop (length sectionHeader - 10) sectionHeader)
        where 
            sectionHeader :: String
            sectionHeader = head row

            sectionLetter :: SectionType -> String
            sectionLetter Reading       = "L"
            sectionLetter Arithmetic    = "R"
            sectionLetter Sol           = "S"
            sectionLetter Numeracy      = "TR"


    parseSchoolInfo :: Section -> Row -> Map.Map String String
    parseSchoolInfo (Section _ grade) row =
        Map.singleton ("skole" ++ (show grade)) (drop 15 (row !! 0))



    goto :: SearchString -> [Row] -> Maybe [Row]
    goto _ [] = Nothing
    goto text (r:rs) = if text `isPrefixOf` head r
                            then Just (r:rs)
                            else goto text rs


    toSearchString :: Cohort -> Section -> String
    toSearchString C2006 (Section Reading 1) = "Leseferdighet 1. trinn (UtgÕtt)"
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

    isEmpty :: Row -> Bool
    isEmpty = (==) [] . head

    dropMiddleNames :: String -> String
    dropMiddleNames name =
        let nameParts = splitOn " " name in
            head nameParts ++ " " ++ last nameParts

    sweep :: [Row] -> [ScaleSpec] -> ([Row], Map.Map String String)
    sweep rows = foldr hubCore (rows, Map.empty) . reverse

    jodlSweep :: [Row] -> DataSpec -> ([Row], Map.Map String String)
    jodlSweep rows (DataSpec cohort sectionSpecs) =        
        foldr (jodlCore cohort) (rows, Map.empty) (reverse sectionSpecs)


    jodlCore :: Cohort -> SectionSpec -> ([Row], Map.Map String String) -> ([Row], Map.Map String String)
    jodlCore cohort (SectionSpec section scaleSpecs) (rows, collectedData) =
        case goto (toSearchString cohort section) rows of
            Nothing         -> (rows, collectedData)
            Just foundRows  -> do
                let testDate = parseTestDate section (foundRows !! 0)
                let school = parseSchoolInfo section (foundRows !! 1)
                let sweepResult = sweep foundRows scaleSpecs
                let newData = foldr Map.union Map.empty [testDate, school, snd sweepResult]
                (fst sweepResult, Map.union collectedData newData)


    hubCore :: ScaleSpec -> ([Row], Map.Map String String) -> ([Row], Map.Map String String)
    hubCore (ScaleSpec searchString dataType vars) (rows, collectedData) =
        case goto searchString rows of
            Nothing -> (rows, collectedData)
            Just foundRows -> do
                let newData = Map.fromList $ validList $ map (parseVal dataType) (zip vars foundRows)
                (drop (length vars) foundRows, Map.union collectedData newData)


    validList :: [Maybe (Variable, String)] -> [(Variable, String)]
    validList [] = []
    validList (Nothing : xs) = validList xs
    validList (Just x : xs)  = x : validList xs


    createVariables :: String -> [Int] -> [String]
    createVariables varBase = map ((++) varBase . show)


    parseVal :: DataType -> (Variable, Row) -> Maybe (Variable, String)
    parseVal dataType =
        case dataType of
            SingleValue ->  Just . parseValue
            CrossGrid ->    parseGrid


    birthdates = Map.fromList []
    