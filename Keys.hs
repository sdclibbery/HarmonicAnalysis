{-|
Module      : Keys
Description : Definition of values for standarda musical Keys
-}
module Keys where
import Key
import Note

-- Major keys

keyOfC = Key C Nat KeyMajor
keyOfCSh = Key C Sh KeyMajor
keyOfD = Key D Nat KeyMajor
keyOfEFl = Key E Fl KeyMajor
keyOfE = Key E Nat KeyMajor
keyOfF = Key F Nat KeyMajor
keyOfFSh = Key F Sh KeyMajor
keyOfG = Key G Nat KeyMajor
keyOfAFl = Key A Fl KeyMajor
keyOfA = Key A Nat KeyMajor
keyOfBFl = Key B Fl KeyMajor
keyOfB = Key B Nat KeyMajor

-- Minor keys

keyOfCMinor = Key C Nat KeyMinor
keyOfCShMinor = Key C Sh KeyMinor
keyOfDMinor = Key D Nat KeyMinor
keyOfDShMinor = Key D Sh KeyMinor
keyOfEMinor = Key E Nat KeyMinor
keyOfFMinor = Key F Nat KeyMinor
keyOfFShMinor = Key F Sh KeyMinor
keyOfGMinor = Key G Nat KeyMinor
keyOfGShMinor = Key G Sh KeyMinor
keyOfAMinor = Key A Nat KeyMinor
keyOfBFlMinor = Key B Fl KeyMinor
keyOfBMinor = Key B Nat KeyMinor
