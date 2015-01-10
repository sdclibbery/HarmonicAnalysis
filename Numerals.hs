{-|
Module      : Numerals
Description : Definition of a set of standard Numerals
-}
module Numerals where
import Note
import Numeral
import Intervals

-- Common major key chords

_I = numeral I Nat [_M3, _P5] First
_I7 = numeral I Nat [_M3, _P5, _m7] First
_Imaj7 = numeral I Nat [_M3, _P5, _M7] First

_ii = numeral II Nat [_m3, _P5] First
_ii7 = numeral II Nat [_m3, _P5, _m7] First

_iii = numeral III Nat [_m3, _P5] First
_iii7 = numeral III Nat [_m3, _P5, _m7] First

_IV = numeral IV Nat [_M3, _P5] First
_IVmaj7 = numeral IV Nat [_M3, _P5, _M7] First

_V = numeral V Nat [_M3, _P5] First
_V7 = numeral V Nat [_M3, _P5, _m7] First
_V7sus4 = numeral V Nat [_P4, _P5, _m7] First

_vi = numeral VI Nat [_m3, _P5] First
_vi7 = numeral VI Nat [_m3, _P5, _m7] First
_vio7 = numeral VI Nat [_m3, _d5, _d7] First

_viio = numeral VII Nat [_m3, _d5] First
_viio7 = numeral VII Nat [_m3, _d5, _d7] First

-- Common minor key chords

_i = numeral I Nat [_m3, _P5] First

_iio = numeral II Nat [_m3, _d5] First
_iio7 = numeral II Nat [_m3, _d5, _d7] First

_III = numeral III Nat [_M3, _P5] First

_iv = numeral IV Nat [_m3, _P5] First
_iv7 = numeral IV Nat [_m3, _P5, _m7] First

_v = numeral V Nat [_m3, _P5] First
_v7 = numeral V Nat [_m3, _P5, _m7] First

_VI = numeral VI Nat [_M3, _P5] First

_VII = numeral VII Nat [_M3, _P5] First

_VII7 = numeral VII Nat [_M3, _P5, _m7] First
