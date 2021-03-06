{-|
Module      : Notes
Description : Lists of note values
-}

module Notes where
import Note
import Structure
import Data.Ratio


-- |Shorthand for one beat (one quarter note) notes of all pitches in many octaves
cf__ = qn $ Note C Fl 2
c__ = qn $ Note C Nat 2
cs__ = qn $ Note C Sh 2
df__ = qn $ Note D Fl 2
d__ = qn $ Note D Nat 2
ds__ = qn $ Note D Sh 2
ef__ = qn $ Note E Fl 2
e__ = qn $ Note E Nat 2
es__ = qn $ Note E Sh 2
ff__ = qn $ Note F Fl 2
f__ = qn $ Note F Nat 2
fs__ = qn $ Note F Sh 2
gf__ = qn $ Note G Fl 2
g__ = qn $ Note G Nat 2
gs__ = qn $ Note G Sh 2
af__ = qn $ Note A Fl 2
a__ = qn $ Note A Nat 2
as__ = qn $ Note A Sh 2
bf__ = qn $ Note B Fl 2
b__ = qn $ Note B Nat 2
bs__ = qn $ Note B Sh 2

cf_ = qn $ Note C Fl 3
c_ = qn $ Note C Nat 3
cs_ = qn $ Note C Sh 3
df_ = qn $ Note D Fl 3
d_ = qn $ Note D Nat 3
ds_ = qn $ Note D Sh 3
ef_ = qn $ Note E Fl 3
e_ = qn $ Note E Nat 3
es_ = qn $ Note E Sh 3
ff_ = qn $ Note F Fl 3
f_ = qn $ Note F Nat 3
fs_ = qn $ Note F Sh 3
gf_ = qn $ Note G Fl 3
g_ = qn $ Note G Nat 3
gs_ = qn $ Note G Sh 3
af_ = qn $ Note A Fl 3
a_ = qn $ Note A Nat 3
as_ = qn $ Note A Sh 3
bf_ = qn $ Note B Fl 3
b_ = qn $ Note B Nat 3
bs_ = qn $ Note B Sh 3

cf = qn $ Note C Fl 4
c = qn $ Note C Nat 4
cs = qn $ Note C Sh 4
df = qn $ Note D Fl 4
d = qn $ Note D Nat 4
ds = qn $ Note D Sh 4
ef = qn $ Note E Fl 4
e = qn $ Note E Nat 4
es = qn $ Note E Sh 4
ff = qn $ Note F Fl 4
f = qn $ Note F Nat 4
fs = qn $ Note F Sh 4
gf = qn $ Note G Fl 4
g = qn $ Note G Nat 4
gs = qn $ Note G Sh 4
af = qn $ Note A Fl 4
a = qn $ Note A Nat 4
as = qn $ Note A Sh 4
bf = qn $ Note B Fl 4
b = qn $ Note B Nat 4
bs = qn $ Note B Sh 4

cf' = qn $ Note C Fl 5
c' = qn $ Note C Nat 5
cs' = qn $ Note C Sh 5
df' = qn $ Note D Fl 5
d' = qn $ Note D Nat 5
ds' = qn $ Note D Sh 5
ef' = qn $ Note E Fl 5
e' = qn $ Note E Nat 5
es' = qn $ Note E Sh 5
ff' = qn $ Note F Fl 5
f' = qn $ Note F Nat 5
fs' = qn $ Note F Sh 5
gf' = qn $ Note G Fl 5
g' = qn $ Note G Nat 5
gs' = qn $ Note G Sh 5
af' = qn $ Note A Fl 5
a' = qn $ Note A Nat 5
as' = qn $ Note A Sh 5
bf' = qn $ Note B Fl 5
b' = qn $ Note B Nat 5
bs' = qn $ Note B Sh 5

cf'' = qn $ Note C Fl 6
c'' = qn $ Note C Nat 6
cs'' = qn $ Note C Sh 6
df'' = qn $ Note D Fl 6
d'' = qn $ Note D Nat 6
ds'' = qn $ Note D Sh 6
ef'' = qn $ Note E Fl 6
e'' = qn $ Note E Nat 6
es'' = qn $ Note E Sh 6
ff'' = qn $ Note F Fl 6
f'' = qn $ Note F Nat 6
fs'' = qn $ Note F Sh 6
gf'' = qn $ Note G Fl 6
g'' = qn $ Note G Nat 6
gs'' = qn $ Note G Sh 6
af'' = qn $ Note A Fl 6
a'' = qn $ Note A Nat 6
as'' = qn $ Note A Sh 6
bf'' = qn $ Note B Fl 6
b'' = qn $ Note B Nat 6
bs'' = qn $ Note B Sh 6


cf__h = Play (1%2) $ Note C Fl 2
c__h = Play (1%2) $ Note C Nat 2
cs__h = Play (1%2) $ Note C Sh 2
df__h = Play (1%2) $ Note D Fl 2
d__h = Play (1%2) $ Note D Nat 2
ds__h = Play (1%2) $ Note D Sh 2
ef__h = Play (1%2) $ Note E Fl 2
e__h = Play (1%2) $ Note E Nat 2
es__h = Play (1%2) $ Note E Sh 2
ff__h = Play (1%2) $ Note F Fl 2
f__h = Play (1%2) $ Note F Nat 2
fs__h = Play (1%2) $ Note F Sh 2
gf__h = Play (1%2) $ Note G Fl 2
g__h = Play (1%2) $ Note G Nat 2
gs__h = Play (1%2) $ Note G Sh 2
af__h = Play (1%2) $ Note A Fl 2
a__h = Play (1%2) $ Note A Nat 2
as__h = Play (1%2) $ Note A Sh 2
bf__h = Play (1%2) $ Note B Fl 2
b__h = Play (1%2) $ Note B Nat 2
bs__h = Play (1%2) $ Note B Sh 2

cf_h = Play (1%2) $ Note C Fl 3
c_h = Play (1%2) $ Note C Nat 3
cs_h = Play (1%2) $ Note C Sh 3
df_h = Play (1%2) $ Note D Fl 3
d_h = Play (1%2) $ Note D Nat 3
ds_h = Play (1%2) $ Note D Sh 3
ef_h = Play (1%2) $ Note E Fl 3
e_h = Play (1%2) $ Note E Nat 3
es_h = Play (1%2) $ Note E Sh 3
ff_h = Play (1%2) $ Note F Fl 3
f_h = Play (1%2) $ Note F Nat 3
fs_h = Play (1%2) $ Note F Sh 3
gf_h = Play (1%2) $ Note G Fl 3
g_h = Play (1%2) $ Note G Nat 3
gs_h = Play (1%2) $ Note G Sh 3
af_h = Play (1%2) $ Note A Fl 3
a_h = Play (1%2) $ Note A Nat 3
as_h = Play (1%2) $ Note A Sh 3
bf_h = Play (1%2) $ Note B Fl 3
b_h = Play (1%2) $ Note B Nat 3
bs_h = Play (1%2) $ Note B Sh 3

cfh = Play (1%2) $ Note C Fl 4
ch = Play (1%2) $ Note C Nat 4
csh = Play (1%2) $ Note C Sh 4
dfh = Play (1%2) $ Note D Fl 4
dh = Play (1%2) $ Note D Nat 4
dsh = Play (1%2) $ Note D Sh 4
efh = Play (1%2) $ Note E Fl 4
eh = Play (1%2) $ Note E Nat 4
esh = Play (1%2) $ Note E Sh 4
ffh = Play (1%2) $ Note F Fl 4
fh = Play (1%2) $ Note F Nat 4
fsh = Play (1%2) $ Note F Sh 4
gfh = Play (1%2) $ Note G Fl 4
gh = Play (1%2) $ Note G Nat 4
gsh = Play (1%2) $ Note G Sh 4
afh = Play (1%2) $ Note A Fl 4
ah = Play (1%2) $ Note A Nat 4
ash = Play (1%2) $ Note A Sh 4
bfh = Play (1%2) $ Note B Fl 4
bh = Play (1%2) $ Note B Nat 4
bsh = Play (1%2) $ Note B Sh 4

cf'h = Play (1%2) $ Note C Fl 5
c'h = Play (1%2) $ Note C Nat 5
cs'h = Play (1%2) $ Note C Sh 5
df'h = Play (1%2) $ Note D Fl 5
d'h = Play (1%2) $ Note D Nat 5
ds'h = Play (1%2) $ Note D Sh 5
ef'h = Play (1%2) $ Note E Fl 5
e'h = Play (1%2) $ Note E Nat 5
es'h = Play (1%2) $ Note E Sh 5
ff'h = Play (1%2) $ Note F Fl 5
f'h = Play (1%2) $ Note F Nat 5
fs'h = Play (1%2) $ Note F Sh 5
gf'h = Play (1%2) $ Note G Fl 5
g'h = Play (1%2) $ Note G Nat 5
gs'h = Play (1%2) $ Note G Sh 5
af'h = Play (1%2) $ Note A Fl 5
a'h = Play (1%2) $ Note A Nat 5
as'h = Play (1%2) $ Note A Sh 5
bf'h = Play (1%2) $ Note B Fl 5
b'h = Play (1%2) $ Note B Nat 5
bs'h = Play (1%2) $ Note B Sh 5

cf''h = Play (1%2) $ Note C Fl 6
c''h = Play (1%2) $ Note C Nat 6
cs''h = Play (1%2) $ Note C Sh 6
df''h = Play (1%2) $ Note D Fl 6
d''h = Play (1%2) $ Note D Nat 6
ds''h = Play (1%2) $ Note D Sh 6
ef''h = Play (1%2) $ Note E Fl 6
e''h = Play (1%2) $ Note E Nat 6
es''h = Play (1%2) $ Note E Sh 6
ff''h = Play (1%2) $ Note F Fl 6
f''h = Play (1%2) $ Note F Nat 6
fs''h = Play (1%2) $ Note F Sh 6
gf''h = Play (1%2) $ Note G Fl 6
g''h = Play (1%2) $ Note G Nat 6
gs''h = Play (1%2) $ Note G Sh 6
af''h = Play (1%2) $ Note A Fl 6
a''h = Play (1%2) $ Note A Nat 6
as''h = Play (1%2) $ Note A Sh 6
bf''h = Play (1%2) $ Note B Fl 6
b''h = Play (1%2) $ Note B Nat 6
bs''h = Play (1%2) $ Note B Sh 6


cf__e = Play (1%8) $ Note C Fl 2
c__e = Play (1%8) $ Note C Nat 2
cs__e = Play (1%8) $ Note C Sh 2
df__e = Play (1%8) $ Note D Fl 2
d__e = Play (1%8) $ Note D Nat 2
ds__e = Play (1%8) $ Note D Sh 2
ef__e = Play (1%8) $ Note E Fl 2
e__e = Play (1%8) $ Note E Nat 2
es__e = Play (1%8) $ Note E Sh 2
ff__e = Play (1%8) $ Note F Fl 2
f__e = Play (1%8) $ Note F Nat 2
fs__e = Play (1%8) $ Note F Sh 2
gf__e = Play (1%8) $ Note G Fl 2
g__e = Play (1%8) $ Note G Nat 2
gs__e = Play (1%8) $ Note G Sh 2
af__e = Play (1%8) $ Note A Fl 2
a__e = Play (1%8) $ Note A Nat 2
as__e = Play (1%8) $ Note A Sh 2
bf__e = Play (1%8) $ Note B Fl 2
b__e = Play (1%8) $ Note B Nat 2
bs__e = Play (1%8) $ Note B Sh 2

cf_e = Play (1%8) $ Note C Fl 3
c_e = Play (1%8) $ Note C Nat 3
cs_e = Play (1%8) $ Note C Sh 3
df_e = Play (1%8) $ Note D Fl 3
d_e = Play (1%8) $ Note D Nat 3
ds_e = Play (1%8) $ Note D Sh 3
ef_e = Play (1%8) $ Note E Fl 3
e_e = Play (1%8) $ Note E Nat 3
es_e = Play (1%8) $ Note E Sh 3
ff_e = Play (1%8) $ Note F Fl 3
f_e = Play (1%8) $ Note F Nat 3
fs_e = Play (1%8) $ Note F Sh 3
gf_e = Play (1%8) $ Note G Fl 3
g_e = Play (1%8) $ Note G Nat 3
gs_e = Play (1%8) $ Note G Sh 3
af_e = Play (1%8) $ Note A Fl 3
a_e = Play (1%8) $ Note A Nat 3
as_e = Play (1%8) $ Note A Sh 3
bf_e = Play (1%8) $ Note B Fl 3
b_e = Play (1%8) $ Note B Nat 3
bs_e = Play (1%8) $ Note B Sh 3

cfe = Play (1%8) $ Note C Fl 4
ce = Play (1%8) $ Note C Nat 4
cse = Play (1%8) $ Note C Sh 4
dfe = Play (1%8) $ Note D Fl 4
de = Play (1%8) $ Note D Nat 4
dse = Play (1%8) $ Note D Sh 4
efe = Play (1%8) $ Note E Fl 4
ee = Play (1%8) $ Note E Nat 4
ese = Play (1%8) $ Note E Sh 4
ffe = Play (1%8) $ Note F Fl 4
fe = Play (1%8) $ Note F Nat 4
fse = Play (1%8) $ Note F Sh 4
gfe = Play (1%8) $ Note G Fl 4
ge = Play (1%8) $ Note G Nat 4
gse = Play (1%8) $ Note G Sh 4
afe = Play (1%8) $ Note A Fl 4
ae = Play (1%8) $ Note A Nat 4
ase = Play (1%8) $ Note A Sh 4
bfe = Play (1%8) $ Note B Fl 4
be = Play (1%8) $ Note B Nat 4
bse = Play (1%8) $ Note B Sh 4

cf'e = Play (1%8) $ Note C Fl 5
c'e = Play (1%8) $ Note C Nat 5
cs'e = Play (1%8) $ Note C Sh 5
df'e = Play (1%8) $ Note D Fl 5
d'e = Play (1%8) $ Note D Nat 5
ds'e = Play (1%8) $ Note D Sh 5
ef'e = Play (1%8) $ Note E Fl 5
e'e = Play (1%8) $ Note E Nat 5
es'e = Play (1%8) $ Note E Sh 5
ff'e = Play (1%8) $ Note F Fl 5
f'e = Play (1%8) $ Note F Nat 5
fs'e = Play (1%8) $ Note F Sh 5
gf'e = Play (1%8) $ Note G Fl 5
g'e = Play (1%8) $ Note G Nat 5
gs'e = Play (1%8) $ Note G Sh 5
af'e = Play (1%8) $ Note A Fl 5
a'e = Play (1%8) $ Note A Nat 5
as'e = Play (1%8) $ Note A Sh 5
bf'e = Play (1%8) $ Note B Fl 5
b'e = Play (1%8) $ Note B Nat 5
bs'e = Play (1%8) $ Note B Sh 5

cf''e = Play (1%8) $ Note C Fl 6
c''e = Play (1%8) $ Note C Nat 6
cs''e = Play (1%8) $ Note C Sh 6
df''e = Play (1%8) $ Note D Fl 6
d''e = Play (1%8) $ Note D Nat 6
ds''e = Play (1%8) $ Note D Sh 6
ef''e = Play (1%8) $ Note E Fl 6
e''e = Play (1%8) $ Note E Nat 6
es''e = Play (1%8) $ Note E Sh 6
ff''e = Play (1%8) $ Note F Fl 6
f''e = Play (1%8) $ Note F Nat 6
fs''e = Play (1%8) $ Note F Sh 6
gf''e = Play (1%8) $ Note G Fl 6
g''e = Play (1%8) $ Note G Nat 6
gs''e = Play (1%8) $ Note G Sh 6
af''e = Play (1%8) $ Note A Fl 6
a''e = Play (1%8) $ Note A Nat 6
as''e = Play (1%8) $ Note A Sh 6
bf''e = Play (1%8) $ Note B Fl 6
b''e = Play (1%8) $ Note B Nat 6
bs''e = Play (1%8) $ Note B Sh 6

