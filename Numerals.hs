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
_Ib = numeral I Nat [_M3, _P5] Second
_Ic = numeral I Nat [_M3, _P5] Third

_I7 = numeral I Nat [_M3, _P5, _m7] First
_I7b = numeral I Nat [_M3, _P5, _m7] Second
_I7c = numeral I Nat [_M3, _P5, _m7] Third
_I7d = numeral I Nat [_M3, _P5, _m7] Fourth

_Imaj7 = numeral I Nat [_M3, _P5, _M7] First
_Imaj7b = numeral I Nat [_M3, _P5, _M7] Second
_Imaj7c = numeral I Nat [_M3, _P5, _M7] Third
_Imaj7d = numeral I Nat [_M3, _P5, _M7] Fourth

_ii = numeral II Nat [_m3, _P5] First
_iib = numeral II Nat [_m3, _P5] Second
_iic = numeral II Nat [_m3, _P5] Third

_ii7 = numeral II Nat [_m3, _P5, _m7] First
_ii7b = numeral II Nat [_m3, _P5, _m7] Second
_ii7c = numeral II Nat [_m3, _P5, _m7] Third
_ii7d = numeral II Nat [_m3, _P5, _m7] Fourth

_iii = numeral III Nat [_m3, _P5] First
_iiib = numeral III Nat [_m3, _P5] Second
_iiic = numeral III Nat [_m3, _P5] Third

_iii7 = numeral III Nat [_m3, _P5, _m7] First
_iii7b = numeral III Nat [_m3, _P5, _m7] Second
_iii7c = numeral III Nat [_m3, _P5, _m7] Third
_iii7d = numeral III Nat [_m3, _P5, _m7] Fourth

_IV = numeral IV Nat [_M3, _P5] First
_IVb = numeral IV Nat [_M3, _P5] Second
_IVc = numeral IV Nat [_M3, _P5] Third

_IVmaj7 = numeral IV Nat [_M3, _P5, _M7] First
_IVmaj7b = numeral IV Nat [_M3, _P5, _M7] Second
_IVmaj7c = numeral IV Nat [_M3, _P5, _M7] Third
_IVmaj7d = numeral IV Nat [_M3, _P5, _M7] Fourth

_V = numeral V Nat [_M3, _P5] First
_Vb = numeral V Nat [_M3, _P5] Second
_Vc = numeral V Nat [_M3, _P5] Third

_V7 = numeral V Nat [_M3, _P5, _m7] First
_V7b = numeral V Nat [_M3, _P5, _m7] Second
_V7c = numeral V Nat [_M3, _P5, _m7] Third
_V7d = numeral V Nat [_M3, _P5, _m7] Fourth

_V7sus4 = numeral V Nat [_P4, _P5, _m7] First
_V7sus4b = numeral V Nat [_P4, _P5, _m7] Second
_V7sus4c = numeral V Nat [_P4, _P5, _m7] Third
_V7sus4d = numeral V Nat [_P4, _P5, _m7] Fourth

_vi = numeral VI Nat [_m3, _P5] First
_vib = numeral VI Nat [_m3, _P5] Second
_vic = numeral VI Nat [_m3, _P5] Third

_vi7 = numeral VI Nat [_m3, _P5, _m7] First
_vi7b = numeral VI Nat [_m3, _P5, _m7] Second
_vi7c = numeral VI Nat [_m3, _P5, _m7] Third
_vi7d = numeral VI Nat [_m3, _P5, _m7] Fourth

_vio7 = numeral VI Nat [_m3, _d5, _d7] First
_vio7b = numeral VI Nat [_m3, _d5, _d7] Second
_vio7c = numeral VI Nat [_m3, _d5, _d7] Third
_vio7d = numeral VI Nat [_m3, _d5, _d7] Fourth

_viio = numeral VII Nat [_m3, _d5] First
_viiob = numeral VII Nat [_m3, _d5] Second
_viioc = numeral VII Nat [_m3, _d5] Third

_viio7 = numeral VII Nat [_m3, _d5, _d7] First
_viio7b = numeral VII Nat [_m3, _d5, _d7] Second
_viio7c = numeral VII Nat [_m3, _d5, _d7] Third
_viio7d = numeral VII Nat [_m3, _d5, _d7] Fourth

-- Common minor key chords

_i = numeral I Nat [_m3, _P5] First
_ib = numeral I Nat [_m3, _P5] Second
_ic = numeral I Nat [_m3, _P5] Third

_iio = numeral II Nat [_m3, _d5] First
_iiob = numeral II Nat [_m3, _d5] Second
_iioc = numeral II Nat [_m3, _d5] Third

_iio7 = numeral II Nat [_m3, _d5, _d7] First
_iio7b = numeral II Nat [_m3, _d5, _d7] Second
_iio7c = numeral II Nat [_m3, _d5, _d7] Third
_iio7d = numeral II Nat [_m3, _d5, _d7] Fourth

_III = numeral III Nat [_M3, _P5] First
_IIIb = numeral III Nat [_M3, _P5] Second
_IIIc = numeral III Nat [_M3, _P5] Third

_iv = numeral IV Nat [_m3, _P5] First
_ivb = numeral IV Nat [_m3, _P5] Second
_ivc = numeral IV Nat [_m3, _P5] Third

_iv7 = numeral IV Nat [_m3, _P5, _m7] First
_iv7b = numeral IV Nat [_m3, _P5, _m7] Second
_iv7c = numeral IV Nat [_m3, _P5, _m7] Third
_iv7d = numeral IV Nat [_m3, _P5, _m7] Fourth

_v = numeral V Nat [_m3, _P5] First
_vb = numeral V Nat [_m3, _P5] Second
_vc = numeral V Nat [_m3, _P5] Third

_v7 = numeral V Nat [_m3, _P5, _m7] First
_v7b = numeral V Nat [_m3, _P5, _m7] Second
_v7c = numeral V Nat [_m3, _P5, _m7] Third
_v7d = numeral V Nat [_m3, _P5, _m7] Fourth

_VI = numeral VI Nat [_M3, _P5] First
_VIb = numeral VI Nat [_M3, _P5] Second
_VIc = numeral VI Nat [_M3, _P5] Third

_VII = numeral VII Nat [_M3, _P5] First
_VIIb = numeral VII Nat [_M3, _P5] Second
_VIIc = numeral VII Nat [_M3, _P5] Third

_VII7 = numeral VII Nat [_M3, _P5, _m7] First
_VII7b = numeral VII Nat [_M3, _P5, _m7] Second
_VII7c = numeral VII Nat [_M3, _P5, _m7] Third
_VII7d = numeral VII Nat [_M3, _P5, _m7] Fourth

-- Secondary dominants

_iiofIV = numeral V Nat [_m3, _P5] First
_iibofIV = numeral V Nat [_m3, _P5] Second
_iicofIV = numeral V Nat [_m3, _P5] Third

_ii7ofIV = numeral V Nat [_m3, _P5, _m7] First
_ii7bofIV = numeral V Nat [_m3, _P5, _m7] Second
_ii7cofIV = numeral V Nat [_m3, _P5, _m7] Third
_ii7dofIV = numeral V Nat [_m3, _P5, _m7] Fourth

_VofV = numeral II Nat [_M3, _P5] First
_VofVb = numeral II Nat [_M3, _P5] Second
_VofVc = numeral II Nat [_M3, _P5] Third

_V7ofV = numeral II Nat [_M3, _P5, _M7] First
_V7ofVb = numeral II Nat [_M3, _P5, _M7] Second
_V7ofVc = numeral II Nat [_M3, _P5, _M7] Third
_V7ofVd = numeral II Nat [_M3, _P5, _M7] Fourth

_viio7ofIV = numeral III Nat [_m3, _d5, _d7] First
_viio7bofIV = numeral III Nat [_m3, _d5, _d7] Second
_viio7cofIV = numeral III Nat [_m3, _d5, _d7] Third
_viio7dofIV = numeral III Nat [_m3, _d5, _d7] Fourth

