{-|
Module      : Intervals
Description : Lists of interval values
-}

module Intervals (
  _d1, _P1, _a1,
  _d2, _m2, _M2, _a2,
  _d3, _m3, _M3, _a3,
  _d4, _P4, _a4,
  _d5, _P5, _a5,
  _d6, _m6, _M6, _a6,
  _d7, _m7, _M7, _a7,
  _d8, _P8, _a8,
  _d9, _m9, _M9, _a9,
  _d10, _m10, _M10, _a10,
  _d11, _P11, _a11,
  _d12, _P12, _a12,
  _d13, _m13, _M13, _a13
) where
import Interval
import Note

_d1 = interval c cf
_P1 = interval c c
_a1 = interval c cs

_d2 = interval c dff
_m2 = interval c df
_M2 = interval c d
_a2 = interval c ds

_d3 = interval c eff
_m3 = interval c ef
_M3 = interval c e
_a3 = interval c es

_d4 = interval c ff
_P4 = interval c f
_a4 = interval c fs

_d5 = interval c gf
_P5 = interval c g
_a5 = interval c gs

_d6 = interval c aff
_m6 = interval c af
_M6 = interval c a
_a6 = interval c as

_d7 = interval c bff
_m7 = interval c bf
_M7 = interval c b
_a7 = interval c bs

_d8 = interval c cf'
_P8 = interval c c'
_a8 = interval c cs'

_d9 = interval c dff'
_m9 = interval c df'
_M9 = interval c d'
_a9 = interval c ds'

_d10 = interval c eff'
_m10 = interval c ef'
_M10 = interval c e'
_a10 = interval c es'

_d11 = interval c ff'
_P11 = interval c f'
_a11 = interval c fs'

_d12 = interval c gf'
_P12 = interval c g'
_a12 = interval c gs'

_d13 = interval c aff'
_m13 = interval c af'
_M13 = interval c a'
_a13 = interval c as'

cf = Note C Fl 4
c = Note C Nat 4
cs = Note C Sh 4

dff = Note D DFl 4
df = Note D Fl 4
d = Note D Nat 4
ds = Note D Sh 4

eff = Note E DFl 4
ef = Note E Fl 4
e = Note E Nat 4
es = Note E Sh 4

ff = Note F Fl 4
f = Note F Nat 4
fs = Note F Sh 4

gf = Note G Fl 4
g = Note G Nat 4
gs = Note G Sh 4

aff = Note A DFl 4
af = Note A Fl 4
a = Note A Nat 4
as = Note A Sh 4

bff = Note B DFl 4
bf = Note B Fl 4
b = Note B Nat 4
bs = Note B Sh 4

cf' = Note C Fl 5
c' = Note C Nat 5
cs' = Note C Sh 5

dff' = Note D DFl 5
df' = Note D Fl 5
d' = Note D Nat 5
ds' = Note D Sh 5

eff' = Note E DFl 5
ef' = Note E Fl 5
e' = Note E Nat 5
es' = Note E Sh 5

ff' = Note F Fl 5
f' = Note F Nat 5
fs' = Note F Sh 5

gf' = Note G Fl 5
g' = Note G Nat 5
gs' = Note G Sh 5

aff' = Note A DFl 5
af' = Note A Fl 5
a' = Note A Nat 5
as' = Note A Sh 5

