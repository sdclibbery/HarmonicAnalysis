module AnnotatedNote (
  ANote (..),
  annotated,
  note
) where
import Note
import Structure

-- |Note annotated with full timing and part info
data ANote = ANote { start :: Time, end :: Time, part :: PartName, event :: Event }

instance Eq ANote where
  n == n' = (note n) == (note n')

instance Ord ANote where
  n `compare` n' = (note n) `compare` (note n')

-- |Convert a part to annotated form
annotated :: Part -> [ANote]
annotated (Part p es) = snd $ foldl ann (0, []) es
  where
    ann (t, as) e = (t + dur e, as ++ [ANote t (t + dur e) p e])
    dur (Rest d) = d
    dur (Play d _) = d

-- |Get a note from an annotated note. This function is PARTIAL!
note :: ANote -> Note
note (ANote _ _ _ (Play _ n)) = n

