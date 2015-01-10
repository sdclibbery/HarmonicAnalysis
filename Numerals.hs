{-|
Module      : Numerals
Description : Definition of a set of standard Numerals
-}
module Numerals where
import Note
import Numeral
import Intervals

-- Common major key chords

_I = numeral I [_M3, _P5] First
_I7 = numeral I [_M3, _P5, _m7] First
_Imaj7 = numeral I [_M3, _P5, _M7] First

_ii = numeral II [_m3, _P5] First
_ii7 = numeral II [_m3, _P5, _m7] First

_iii = numeral III [_m3, _P5] First
_iii7 = numeral III [_m3, _P5, _m7] First

_IV = numeral IV [_M3, _P5] First
_IVmaj7 = numeral IV [_M3, _P5, _M7] First

_V = numeral V [_M3, _P5] First
_V7 = numeral V [_M3, _P5, _m7] First
_V7sus4 = numeral V [_P4, _P5, _m7] First

_vi = numeral VI [_m3, _P5] First
_vi7 = numeral VI [_m3, _P5, _m7] First
_vio7 = numeral VI [_m3, _d5, _d7] First

_viio = numeral VII [_m3, _d5] First
_viio7 = numeral VII [_m3, _d5, _d7] First

-- Common minor key chords

_i = numeral I [_m3, _P5] First

_iio = numeral II [_m3, _d5] First
_iio7 = numeral II [_m3, _d5, _d7] First

_III = numeral III [_M3, _P5] First

_iv = numeral IV [_m3, _P5] First
_iv7 = numeral IV [_m3, _P5, _m7] First

_v = numeral V [_m3, _P5] First
_v7 = numeral V [_m3, _P5, _m7] First

_VI = numeral VI [_M3, _P5] First

_VII = numeral VII [_M3, _P5] First

_VII7 = numeral VII [_M3, _P5, _m7] First
