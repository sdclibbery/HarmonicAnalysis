{-|
Module      : Numerals
Description : Definition of a set of standard Numerals
-}
module Numerals where
import Note
import Numeral
import Intervals

-- Common major key chords

_I = Numeral I Nat [_M3, _P5] First
_Ib = Numeral I Nat [_M3, _P5] Second
_Ic = Numeral I Nat [_M3, _P5] Third

_I7 = Numeral I Nat [_M3, _P5, _m7] First
_I7b = Numeral I Nat [_M3, _P5, _m7] Second
_I7c = Numeral I Nat [_M3, _P5, _m7] Third
_I7d = Numeral I Nat [_M3, _P5, _m7] Fourth

_Imaj7 = Numeral I Nat [_M3, _P5, _M7] First
_Imaj7b = Numeral I Nat [_M3, _P5, _M7] Second
_Imaj7c = Numeral I Nat [_M3, _P5, _M7] Third
_Imaj7d = Numeral I Nat [_M3, _P5, _M7] Fourth

_ii = Numeral II Nat [_m3, _P5] First
_iib = Numeral II Nat [_m3, _P5] Second
_iic = Numeral II Nat [_m3, _P5] Third

_ii7 = Numeral II Nat [_m3, _P5, _m7] First
_ii7b = Numeral II Nat [_m3, _P5, _m7] Second
_ii7c = Numeral II Nat [_m3, _P5, _m7] Third
_ii7d = Numeral II Nat [_m3, _P5, _m7] Fourth

_iii = Numeral III Nat [_m3, _P5] First
_iiib = Numeral III Nat [_m3, _P5] Second
_iiic = Numeral III Nat [_m3, _P5] Third

_iii7 = Numeral III Nat [_m3, _P5, _m7] First
_iii7b = Numeral III Nat [_m3, _P5, _m7] Second
_iii7c = Numeral III Nat [_m3, _P5, _m7] Third
_iii7d = Numeral III Nat [_m3, _P5, _m7] Fourth

_IV = Numeral IV Nat [_M3, _P5] First
_IVb = Numeral IV Nat [_M3, _P5] Second
_IVc = Numeral IV Nat [_M3, _P5] Third

_IVmaj7 = Numeral IV Nat [_M3, _P5, _M7] First
_IVmaj7b = Numeral IV Nat [_M3, _P5, _M7] Second
_IVmaj7c = Numeral IV Nat [_M3, _P5, _M7] Third
_IVmaj7d = Numeral IV Nat [_M3, _P5, _M7] Fourth

_V = Numeral V Nat [_M3, _P5] First
_Vb = Numeral V Nat [_M3, _P5] Second
_Vc = Numeral V Nat [_M3, _P5] Third

_V7 = Numeral V Nat [_M3, _P5, _m7] First
_V7b = Numeral V Nat [_M3, _P5, _m7] Second
_V7c = Numeral V Nat [_M3, _P5, _m7] Third
_V7d = Numeral V Nat [_M3, _P5, _m7] Fourth

_V7sus4 = Numeral V Nat [_P4, _P5, _m7] First
_V7sus4b = Numeral V Nat [_P4, _P5, _m7] Second
_V7sus4c = Numeral V Nat [_P4, _P5, _m7] Third
_V7sus4d = Numeral V Nat [_P4, _P5, _m7] Fourth

_vi = Numeral VI Nat [_m3, _P5] First
_vib = Numeral VI Nat [_m3, _P5] Second
_vic = Numeral VI Nat [_m3, _P5] Third

_vi7 = Numeral VI Nat [_m3, _P5, _m7] First
_vi7b = Numeral VI Nat [_m3, _P5, _m7] Second
_vi7c = Numeral VI Nat [_m3, _P5, _m7] Third
_vi7d = Numeral VI Nat [_m3, _P5, _m7] Fourth

_vio7 = Numeral VI Nat [_m3, _d5, _d7] First
_vio7b = Numeral VI Nat [_m3, _d5, _d7] Second
_vio7c = Numeral VI Nat [_m3, _d5, _d7] Third
_vio7d = Numeral VI Nat [_m3, _d5, _d7] Fourth

_viio = Numeral VII Nat [_m3, _d5] First
_viiob = Numeral VII Nat [_m3, _d5] Second
_viioc = Numeral VII Nat [_m3, _d5] Third

_viio7 = Numeral VII Nat [_m3, _d5, _d7] First
_viio7b = Numeral VII Nat [_m3, _d5, _d7] Second
_viio7c = Numeral VII Nat [_m3, _d5, _d7] Third
_viio7d = Numeral VII Nat [_m3, _d5, _d7] Fourth

-- Common minor key chords

_i = Numeral I Nat [_m3, _P5] First
_ib = Numeral I Nat [_m3, _P5] Second
_ic = Numeral I Nat [_m3, _P5] Third

_iio = Numeral II Nat [_m3, _d5] First
_iiob = Numeral II Nat [_m3, _d5] Second
_iioc = Numeral II Nat [_m3, _d5] Third

_iio7 = Numeral II Nat [_m3, _d5, _d7] First
_iio7b = Numeral II Nat [_m3, _d5, _d7] Second
_iio7c = Numeral II Nat [_m3, _d5, _d7] Third
_iio7d = Numeral II Nat [_m3, _d5, _d7] Fourth

_III = Numeral III Nat [_M3, _P5] First
_IIIb = Numeral III Nat [_M3, _P5] Second
_IIIc = Numeral III Nat [_M3, _P5] Third

_iv = Numeral IV Nat [_m3, _P5] First
_ivb = Numeral IV Nat [_m3, _P5] Second
_ivc = Numeral IV Nat [_m3, _P5] Third

_iv7 = Numeral IV Nat [_m3, _P5, _m7] First
_iv7b = Numeral IV Nat [_m3, _P5, _m7] Second
_iv7c = Numeral IV Nat [_m3, _P5, _m7] Third
_iv7d = Numeral IV Nat [_m3, _P5, _m7] Fourth

_v = Numeral V Nat [_m3, _P5] First
_vb = Numeral V Nat [_m3, _P5] Second
_vc = Numeral V Nat [_m3, _P5] Third

_v7 = Numeral V Nat [_m3, _P5, _m7] First
_v7b = Numeral V Nat [_m3, _P5, _m7] Second
_v7c = Numeral V Nat [_m3, _P5, _m7] Third
_v7d = Numeral V Nat [_m3, _P5, _m7] Fourth

_VI = Numeral VI Nat [_M3, _P5] First
_VIb = Numeral VI Nat [_M3, _P5] Second
_VIc = Numeral VI Nat [_M3, _P5] Third

_VII = Numeral VII Nat [_M3, _P5] First
_VIIb = Numeral VII Nat [_M3, _P5] Second
_VIIc = Numeral VII Nat [_M3, _P5] Third

_VII7 = Numeral VII Nat [_M3, _P5, _m7] First
_VII7b = Numeral VII Nat [_M3, _P5, _m7] Second
_VII7c = Numeral VII Nat [_M3, _P5, _m7] Third
_VII7d = Numeral VII Nat [_M3, _P5, _m7] Fourth

-- Secondary dominants

_iiofIV = Numeral V Nat [_m3, _P5] First
_iibofIV = Numeral V Nat [_m3, _P5] Second
_iicofIV = Numeral V Nat [_m3, _P5] Third

_ii7ofIV = Numeral V Nat [_m3, _P5, _m7] First
_ii7bofIV = Numeral V Nat [_m3, _P5, _m7] Second
_ii7cofIV = Numeral V Nat [_m3, _P5, _m7] Third
_ii7dofIV = Numeral V Nat [_m3, _P5, _m7] Fourth

_VofV = Numeral II Nat [_M3, _P5] First
_VofVb = Numeral II Nat [_M3, _P5] Second
_VofVc = Numeral II Nat [_M3, _P5] Third

_V7ofV = Numeral II Nat [_M3, _P5, _M7] First
_V7ofVb = Numeral II Nat [_M3, _P5, _M7] Second
_V7ofVc = Numeral II Nat [_M3, _P5, _M7] Third
_V7ofVd = Numeral II Nat [_M3, _P5, _M7] Fourth

_viio7ofIV = Numeral III Nat [_m3, _d5, _d7] First
_viio7bofIV = Numeral III Nat [_m3, _d5, _d7] Second
_viio7cofIV = Numeral III Nat [_m3, _d5, _d7] Third
_viio7dofIV = Numeral III Nat [_m3, _d5, _d7] Fourth

