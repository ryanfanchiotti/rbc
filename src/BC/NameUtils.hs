module BC.NameUtils (
    makeName,
    makeLabel,
    makeNameState,
    NameState(..)
) where

-- Names are non negative integers that count upwards
type NameState = Int

makeNameState :: NameState
makeNameState = 0

-- Names in programs cannot start with a dot, therefore
-- all generated names that do are valid 
makeName :: NameState -> (String, NameState)
makeName ns = ("_N" ++ (show ns), ns + 1)

makeLabel :: NameState -> (String, NameState)
makeLabel ns = (".L" ++ (show ns), ns + 1)

