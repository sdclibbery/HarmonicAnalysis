import Structure
import Note
import Interval
import Notes
import Midi
import Melody
import Harmony
import Data.List

thing = music $ map (.>>2) [
		[	c_h,		d_,	c_,	f_e,e_e,d_,	g_h,		c_h	],
		[	c,	r,	a_,	r,	d,	r,	d,	r,	g_h	],
		[	gh,		c',	g,	r,	a,	b,	b,	eh	],
		[	e',	f'h,		be,c'e,d',	f',	g',	e'e,f'e,c'h	]
	]

main = do
	putStrLn $ intercalate "\n" $ map show $ Melody.analyse thing
	putStrLn $ intercalate "\n" $ map show $ Harmony.analyse thing
	createMidi "test.midi" $ thing

