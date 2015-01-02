{-|
Module      : Numerals
Description : Definition of a set of standard Numerals
-}
module Numerals where
import Note
import Numeral
import Intervals

_I = Numeral I Nat [_M3, _P5] First
_Ib = Numeral I Nat [_M3, _P5] Second
_Ic = Numeral I Nat [_M3, _P5] Third

_ii = Numeral II Nat [_m3, _P5] First
_ii7d = Numeral II Nat [_m3, _P5, _m7] Fourth

_V = Numeral V Nat [_M3, _P5] First
_V7 = Numeral V Nat [_M3, _P5, _m7] First
_V7b = Numeral V Nat [_M3, _P5, _m7] Second

