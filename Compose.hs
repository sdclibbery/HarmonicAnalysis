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
(Play d n) .> i = Play (d * (i%1)) n
(Rest d) .> i = Rest (d * (i%1))

-- |Lengthen a list of notes or rests by a given multiplier. Eg .>>2 doubles all the events duration.
(.>>) :: [Event] -> Integer -> [Event]
es .>> i = map (.> i) es


-- |Shorten a note or rest by a given multiplier. Eg .<2 halves the events duration.
(.<) :: Event -> Integer -> Event
(Play d n) .< i = Play (d * (1%i)) n
(Rest d) .< i = Rest (d * (1%i))

-- |Shorten a list of notes or rests by a given multiplier. Eg .<<2 halves all the events duration.
(.<<) :: [Event] -> Integer -> [Event]
es .<< i = map (.< i) es


-- |Make a music from lists of events. Each list of events is assigned a default part name
music :: [[Event]] -> Music
music ess
  | length ess == 1 = Music $ map makePart $ zip [Bass] ess
  | length ess == 2 = Music $ map makePart $ zip [Bass, Treble] ess
  | length ess == 3 = Music $ map makePart $ zip [Bass, Alto, Treble] ess
  | length ess == 4 = Music $ map makePart $ zip [Bass, Tenor, Alto, Treble] ess
  where
    makePart (n, es) = Part n es


-- |Shorthand for a one beat (one quarter note) rest
r = Rest (1%4)


-- |Shorthand for one beat (one quarter note) notes of all pitches in many octaves
cf__ = Play (1%4) $ Note C Fl 2
c__ = Play (1%4) $ Note C Nat 2
cs__ = Play (1%4) $ Note C Sh 2
df__ = Play (1%4) $ Note D Fl 2
d__ = Play (1%4) $ Note D Nat 2
ds__ = Play (1%4) $ Note D Sh 2
ef__ = Play (1%4) $ Note E Fl 2
e__ = Play (1%4) $ Note E Nat 2
es__ = Play (1%4) $ Note E Sh 2
ff__ = Play (1%4) $ Note F Fl 2
f__ = Play (1%4) $ Note F Nat 2
fs__ = Play (1%4) $ Note F Sh 2
gf__ = Play (1%4) $ Note G Fl 2
g__ = Play (1%4) $ Note G Nat 2
gs__ = Play (1%4) $ Note G Sh 2
af__ = Play (1%4) $ Note A Fl 2
a__ = Play (1%4) $ Note A Nat 2
as__ = Play (1%4) $ Note A Sh 2
bf__ = Play (1%4) $ Note B Fl 2
b__ = Play (1%4) $ Note B Nat 2
bs__ = Play (1%4) $ Note B Sh 2

cf_ = Play (1%4) $ Note C Fl 3
c_ = Play (1%4) $ Note C Nat 3
cs_ = Play (1%4) $ Note C Sh 3
df_ = Play (1%4) $ Note D Fl 3
d_ = Play (1%4) $ Note D Nat 3
ds_ = Play (1%4) $ Note D Sh 3
ef_ = Play (1%4) $ Note E Fl 3
e_ = Play (1%4) $ Note E Nat 3
es_ = Play (1%4) $ Note E Sh 3
ff_ = Play (1%4) $ Note F Fl 3
f_ = Play (1%4) $ Note F Nat 3
fs_ = Play (1%4) $ Note F Sh 3
gf_ = Play (1%4) $ Note G Fl 3
g_ = Play (1%4) $ Note G Nat 3
gs_ = Play (1%4) $ Note G Sh 3
af_ = Play (1%4) $ Note A Fl 3
a_ = Play (1%4) $ Note A Nat 3
as_ = Play (1%4) $ Note A Sh 3
bf_ = Play (1%4) $ Note B Fl 3
b_ = Play (1%4) $ Note B Nat 3
bs_ = Play (1%4) $ Note B Sh 3

cf = Play (1%4) $ Note C Fl 4
c = Play (1%4) $ Note C Nat 4
cs = Play (1%4) $ Note C Sh 4
df = Play (1%4) $ Note D Fl 4
d = Play (1%4) $ Note D Nat 4
ds = Play (1%4) $ Note D Sh 4
ef = Play (1%4) $ Note E Fl 4
e = Play (1%4) $ Note E Nat 4
es = Play (1%4) $ Note E Sh 4
ff = Play (1%4) $ Note F Fl 4
f = Play (1%4) $ Note F Nat 4
fs = Play (1%4) $ Note F Sh 4
gf = Play (1%4) $ Note G Fl 4
g = Play (1%4) $ Note G Nat 4
gs = Play (1%4) $ Note G Sh 4
af = Play (1%4) $ Note A Fl 4
a = Play (1%4) $ Note A Nat 4
as = Play (1%4) $ Note A Sh 4
bf = Play (1%4) $ Note B Fl 4
b = Play (1%4) $ Note B Nat 4
bs = Play (1%4) $ Note B Sh 4

cf' = Play (1%4) $ Note C Fl 5
c' = Play (1%4) $ Note C Nat 5
cs' = Play (1%4) $ Note C Sh 5
df' = Play (1%4) $ Note D Fl 5
d' = Play (1%4) $ Note D Nat 5
ds' = Play (1%4) $ Note D Sh 5
ef' = Play (1%4) $ Note E Fl 5
e' = Play (1%4) $ Note E Nat 5
es' = Play (1%4) $ Note E Sh 5
ff' = Play (1%4) $ Note F Fl 5
f' = Play (1%4) $ Note F Nat 5
fs' = Play (1%4) $ Note F Sh 5
gf' = Play (1%4) $ Note G Fl 5
g' = Play (1%4) $ Note G Nat 5
gs' = Play (1%4) $ Note G Sh 5
af' = Play (1%4) $ Note A Fl 5
a' = Play (1%4) $ Note A Nat 5
as' = Play (1%4) $ Note A Sh 5
bf' = Play (1%4) $ Note B Fl 5
b' = Play (1%4) $ Note B Nat 5
bs' = Play (1%4) $ Note B Sh 5

cf'' = Play (1%4) $ Note C Fl 6
c'' = Play (1%4) $ Note C Nat 6
cs'' = Play (1%4) $ Note C Sh 6
df'' = Play (1%4) $ Note D Fl 6
d'' = Play (1%4) $ Note D Nat 6
ds'' = Play (1%4) $ Note D Sh 6
ef'' = Play (1%4) $ Note E Fl 6
e'' = Play (1%4) $ Note E Nat 6
es'' = Play (1%4) $ Note E Sh 6
ff'' = Play (1%4) $ Note F Fl 6
f'' = Play (1%4) $ Note F Nat 6
fs'' = Play (1%4) $ Note F Sh 6
gf'' = Play (1%4) $ Note G Fl 6
g'' = Play (1%4) $ Note G Nat 6
gs'' = Play (1%4) $ Note G Sh 6
af'' = Play (1%4) $ Note A Fl 6
a'' = Play (1%4) $ Note A Nat 6
as'' = Play (1%4) $ Note A Sh 6
bf'' = Play (1%4) $ Note B Fl 6
b'' = Play (1%4) $ Note B Nat 6
bs'' = Play (1%4) $ Note B Sh 6

