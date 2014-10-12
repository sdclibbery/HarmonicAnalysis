{-|
Module      : Compose
Description : Helpers to make composition easier
-}

module Compose where
import Note
import Structure
import Data.Ratio


-- |Lengthen a note or rest by a given multiplier. Eg .>2 doubles the events duration.
(.>) :: Event -> Integer -> Event
(Note d n) .> i = Note (d * (i%1)) n
(Rest d) .> i = Rest (d * (i%1))

-- |Lengthen a list of notes or rests by a given multiplier. Eg .>>2 doubles all the events duration.
(.>>) :: [Event] -> Integer -> [Event]
es .>> i = map (.> i) es


-- |Shorten a note or rest by a given multiplier. Eg .<2 halves the events duration.
(.<) :: Event -> Integer -> Event
(Note d n) .< i = Note (d * (1%i)) n
(Rest d) .< i = Rest (d * (1%i))

-- |Shorten a list of notes or rests by a given multiplier. Eg .<<2 halves all the events duration.
(.<<) :: [Event] -> Integer -> [Event]
es .<< i = map (.< i) es


-- |Make a music from lists of events. Each list of events is assigned a default part name
music :: [[Event]] -> Music
music ess = Music $ map makePart $ zip ["", "2", "3", "4"] ess
  where
    makePart (n, es) = Part ("p"++n) es


-- |Shorthand for a one beat (one quarter note) rest
r = Rest (1%4)


-- |Shorthand for one beat (one quarter note) notes of all pitches in many octaves
cf__ = Note (1%4) $ C Fl 2
c__ = Note (1%4) $ C Nat 2
cs__ = Note (1%4) $ C Sh 2
df__ = Note (1%4) $ D Fl 2
d__ = Note (1%4) $ D Nat 2
ds__ = Note (1%4) $ D Sh 2
ef__ = Note (1%4) $ E Fl 2
e__ = Note (1%4) $ E Nat 2
es__ = Note (1%4) $ E Sh 2
ff__ = Note (1%4) $ F Fl 2
f__ = Note (1%4) $ F Nat 2
fs__ = Note (1%4) $ F Sh 2
gf__ = Note (1%4) $ G Fl 2
g__ = Note (1%4) $ G Nat 2
gs__ = Note (1%4) $ G Sh 2
af__ = Note (1%4) $ A Fl 2
a__ = Note (1%4) $ A Nat 2
as__ = Note (1%4) $ A Sh 2
bf__ = Note (1%4) $ B Fl 2
b__ = Note (1%4) $ B Nat 2
bs__ = Note (1%4) $ B Sh 2

cf_ = Note (1%4) $ C Fl 3
c_ = Note (1%4) $ C Nat 3
cs_ = Note (1%4) $ C Sh 3
df_ = Note (1%4) $ D Fl 3
d_ = Note (1%4) $ D Nat 3
ds_ = Note (1%4) $ D Sh 3
ef_ = Note (1%4) $ E Fl 3
e_ = Note (1%4) $ E Nat 3
es_ = Note (1%4) $ E Sh 3
ff_ = Note (1%4) $ F Fl 3
f_ = Note (1%4) $ F Nat 3
fs_ = Note (1%4) $ F Sh 3
gf_ = Note (1%4) $ G Fl 3
g_ = Note (1%4) $ G Nat 3
gs_ = Note (1%4) $ G Sh 3
af_ = Note (1%4) $ A Fl 3
a_ = Note (1%4) $ A Nat 3
as_ = Note (1%4) $ A Sh 3
bf_ = Note (1%4) $ B Fl 3
b_ = Note (1%4) $ B Nat 3
bs_ = Note (1%4) $ B Sh 3

cf = Note (1%4) $ C Fl 4
c = Note (1%4) $ C Nat 4
cs = Note (1%4) $ C Sh 4
df = Note (1%4) $ D Fl 4
d = Note (1%4) $ D Nat 4
ds = Note (1%4) $ D Sh 4
ef = Note (1%4) $ E Fl 4
e = Note (1%4) $ E Nat 4
es = Note (1%4) $ E Sh 4
ff = Note (1%4) $ F Fl 4
f = Note (1%4) $ F Nat 4
fs = Note (1%4) $ F Sh 4
gf = Note (1%4) $ G Fl 4
g = Note (1%4) $ G Nat 4
gs = Note (1%4) $ G Sh 4
af = Note (1%4) $ A Fl 4
a = Note (1%4) $ A Nat 4
as = Note (1%4) $ A Sh 4
bf = Note (1%4) $ B Fl 4
b = Note (1%4) $ B Nat 4
bs = Note (1%4) $ B Sh 4

cf' = Note (1%4) $ C Fl 5
c' = Note (1%4) $ C Nat 5
cs' = Note (1%4) $ C Sh 5
df' = Note (1%4) $ D Fl 5
d' = Note (1%4) $ D Nat 5
ds' = Note (1%4) $ D Sh 5
ef' = Note (1%4) $ E Fl 5
e' = Note (1%4) $ E Nat 5
es' = Note (1%4) $ E Sh 5
ff' = Note (1%4) $ F Fl 5
f' = Note (1%4) $ F Nat 5
fs' = Note (1%4) $ F Sh 5
gf' = Note (1%4) $ G Fl 5
g' = Note (1%4) $ G Nat 5
gs' = Note (1%4) $ G Sh 5
af' = Note (1%4) $ A Fl 5
a' = Note (1%4) $ A Nat 5
as' = Note (1%4) $ A Sh 5
bf' = Note (1%4) $ B Fl 5
b' = Note (1%4) $ B Nat 5
bs' = Note (1%4) $ B Sh 5

cf'' = Note (1%4) $ C Fl 6
c'' = Note (1%4) $ C Nat 6
cs'' = Note (1%4) $ C Sh 6
df'' = Note (1%4) $ D Fl 6
d'' = Note (1%4) $ D Nat 6
ds'' = Note (1%4) $ D Sh 6
ef'' = Note (1%4) $ E Fl 6
e'' = Note (1%4) $ E Nat 6
es'' = Note (1%4) $ E Sh 6
ff'' = Note (1%4) $ F Fl 6
f'' = Note (1%4) $ F Nat 6
fs'' = Note (1%4) $ F Sh 6
gf'' = Note (1%4) $ G Fl 6
g'' = Note (1%4) $ G Nat 6
gs'' = Note (1%4) $ G Sh 6
af'' = Note (1%4) $ A Fl 6
a'' = Note (1%4) $ A Nat 6
as'' = Note (1%4) $ A Sh 6
bf'' = Note (1%4) $ B Fl 6
b'' = Note (1%4) $ B Nat 6
bs'' = Note (1%4) $ B Sh 6

