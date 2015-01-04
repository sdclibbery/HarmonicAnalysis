import Structure
import Note
import Interval
import Intervals
import Notes
import Midi
import Harmony
import Melody
import Key
import Keys
import Chord
import Numeral
import Numerals
import Data.Ratio


type Progression = [(Key, [Numeral])]

progressionToChords :: Progression -> [Chord]
progressionToChords = concatMap numeralsToChords
  where
    numeralsToChords (k, ns) = map (numeralToChord k) ns


-- ?? No difference between V7sus4 and V7??
-- ?? Strange gaps/silences in places?
-- Chord should have smart constructors and limited exports so it can enforce constraints: Intervals should all be normalised and sorted
-- Numeral should have smart constructors and limited exports so it can enforce constraints: Intervals should all be normalised and sorted
-- Next: mechanisms for expanding chords out into parts that obey voice leading and part writing rules...


-- C Major prelude - Book one, well tempered clavier
progression :: Progression
progression = [
    (keyOfC, [ _I, _ii7d, _V7b, _I, _vib, _V7ofVd ]),
    (keyOfG, [_Ib, _ii7, _V7, _I, _vio7, _iicofIV, _viio7cofIV]),
    (keyOfC, [_Ib, _IVmaj7d, _ii7, _V7, _I, _I7, _IVmaj7, _vio7d, _viio7d, _V7, _Ic, _V7sus4, _V7, _vio7add7e, _Ic, _V7sus4, _V7, _I7])
    ]
  where
    _vio7add7e = Numeral VI Nat [_m3, _d5, _M6, _m7] Fifth

coda = (
    [c__, c__, c__].>>4,
    [r.<4, c_.>15.<4, r.<4, c_.>15.<4, c_.>4],
    [r.>2, f_, a_, c, f, c_, a_, c_, a_, f_, a_, f_, d_, f_, d_, r.>2, g, b, d', f', d', b, d', b, g, b, d, f, e, d].<<4 ++ [c'.>4]
  )

twice :: [a] -> [a]
twice xs = xs ++ xs

type Parts = ([Event], [Event], [Event])

concatParts :: Parts -> Parts -> Parts
concatParts (b, t, tr) (b', t', tr') = (b++b', t++t', tr++tr')

chordToParts :: [Event] -> Parts
chordToParts (ba:te:trs) = (bass, tenor, treble)
  where
    bass = twice [ba.>2]
    tenor = twice [r.<4, te.>7.<4]
    treble = twice (r.<2 : twice (trs.<<4))

extendTo5Notes :: [Note] -> [Note]
extendTo5Notes (a:b:c:[]) = a:b:c:up a:up b:[]
extendTo5Notes (a:b:c:d:[]) = a:b:c:d:up a:[]
extendTo5Notes (a:b:c:d:e:[]) = a:b:c:d:e:[]
up = modifyOctave 1

prelude = music $ map (.>>2) [ bass, tenor, treble ]
  where
    (bass, tenor, treble) = concatParts body coda
    body = foldr (concatParts.chordToParts) ([],[],[]) $ map (map qn . extendTo5Notes . chordToNotes) $ progressionToChords progression
    concatParts (bs',ts',trs') (bs,ts,trs) = (bs'++bs, ts'++ts, trs'++trs)


main = createMidi "test.midi" $ prelude

--main = putStrLn $ show $ Melody.analyse prelude


--main = createMidi "test.midi" $ music [ [c, d].>>2, [e, f].>>2, [g, a].>>2 ]
--main = createMidi "test.midi" $ music [ [c, d, g, a, c', r, b, a, g, f, d, b_, c.>4] ]


