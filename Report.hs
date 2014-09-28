module Report where

data Ref = Harmony Int deriving (Eq, Show)

data Context = Context Int Int deriving (Eq, Show)

data Report = Warning Ref Context String | Error Ref Context String deriving (Eq, Show)

