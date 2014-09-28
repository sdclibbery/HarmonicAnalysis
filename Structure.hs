module Structure where
import Note

data Event = Rest | Note Note deriving (Eq, Show)

data Beat = Beat { event :: Event, incidental :: [Note] } deriving (Eq, Show)

data Music = Music [Beat] deriving (Eq, Show)

