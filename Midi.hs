{-|
Module      : Midi
Description : Write a Music out as a midi file
-}

module Midi (
  createMidi
) where
import Codec.Midi
import Data.List
import Data.Ratio
import Structure
import Note


-- |Create a midi file representing the given Music
createMidi :: FilePath -> Music -> IO()
createMidi f (Music ps) = exportFile f $ Midi {
         fileType = MultiTrack, 
         timeDiv = TicksPerBeat ticksPerBeat, 
         tracks = map makeTrack ps
       }


type Pitch = Int

type MidiEvent = (Ticks, Message)

makeTrack :: Part -> (Track Ticks)
makeTrack (Part p es) = [
   (0,ChannelPrefix 0),
   (0,TrackName " Grand Piano  "),
   (0,InstrumentName "GM Device  1"),
   (0,TimeSignature 4 2 24 8),
   (0,KeySignature 0 0)
  ]
  ++
  concatMap playEvent es
  ++
  [
   (1000,TrackEnd)
  ]

ticksPerBeat :: Int
ticksPerBeat = 480

beatsPerBar :: Int
beatsPerBar = 4

toTicks :: Structure.Time -> Ticks
toTicks t = truncate $ (fromIntegral $ ticksPerBeat * beatsPerBar) * (fromRational t)

playEvent :: Event -> (Track Ticks)
playEvent (Rest d) = [(toTicks d, NoteOff {channel = 0, key = 0, velocity = 0})] -- Bit hacky but it works; a resxt really delay the start of the following note
playEvent (Play d n) = playnote (absChromatic n) d

playnote :: Pitch -> Structure.Time -> Track Ticks
playnote k d = [keydown k, keyup k]
  where
    keydown k = (0, NoteOn {channel = 0, key = k, velocity = 120})
    keyup k = (toTicks d, NoteOn {channel = 0, key = k, velocity = 0})

