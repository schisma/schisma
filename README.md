# Schisma

Born out of frustration with DAWs and traditional sheet music software, Schisma
is a lightweight, feature-rich composition tool. From notation to sound design,
Schisma aims to give complete control of the composition process to the
musician.

# Features

* Ability to create and use complex synthesizers
* Ability to use custom temperaments and tuning systems
* Support for custom input formats
* MIDI interoperability
* Lightweight text format, allowing for portability and the use of a wide range
  of editing tools
* Type-safe sound design and instrument creation

# Getting Started

You'll need to install [Csound](https://csound.com/download.html),
[PortMidi](http://portmedia.sourceforge.net/), and then
[Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install).

After that's done, make sure to add `~/.local/bin` to your `$PATH`, and then 
run the following from the project root:

```sh
stack install
```

That should place the `schisma` executable within your `$PATH`.

# Input Methods

Schisma aims to support a variety of input methods. At the heart of each input 
method is an instruments file.

## Instruments File Format

An instruments file is simple JSON. Here's an excerpt from the 
[Commemoration example](https://gitlab.com/schisma/schisma/-/blob/master/examples/Commemoration/instruments.json):

```json
{
  "instruments": [
    {
      "number": 1,
      "name": "QBSA",
      "instrument": "Profit",
      "soundFontPath": "",
      "parameters": {
        "amplitude"                           : 0.63,
        "polyModSourceAmountFilterEnvelope"   : 0,
        "polyModSourceAmountOscillatorB"      : 0,
        "polyModDestinationFrequencyA"        : 0,
        "polyModDestinationPulseWidthA"       : 0,
        "polyModDestinationFilter"            : 0,
        ...
      },
      "midiChannel": 1
    }
  ]
}
```

The top-level keys are:

* `instruments` - an array of instrument definitions whose keys are:
  + `number` - a number by which to identify it in the [tracker file](#tracker-file-format)
  + `name` - a name
  + `instrument` - the [instrument](#available-instruments) to be used
  + `soundFontPath` - the path to the SoundFont file should `instrument` be 
    "SoundFont", blank otherwise
  + `parameters` - an object containing the instrument [parameters](#parameters) 
    along with their corresponding values
  + `midiChannel` - the MIDI channel (1-16) if [MIDI interop](#midi) is desired,
    or 0 to disable

## Tracker

Schisma's tracker derives inspiration from popular trackers like 
[Renoise](https://www.renoise.com/). Contrary to most trackers, Schisma's 
tracker interface is just a `.csv` file. This makes for easy editing within a
variety of applications. 

### Tracker File Format

Here's an excerpt from the 
[Commemoration example](https://gitlab.com/schisma/schisma/-/blob/master/examples/Commemoration/tracker.csv):

```csv
#,Master,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA,I1 QBSA
0,bpm:60 lpb:1,F#2,,,D5 amp:0.2,,,,,,,,
1,,,,,,,,,,,,,
2,,,,,,,,,,,,,
3,,,,,C#5 amp:0.2,,,,,,,,
----------,----------,----------,----------,----------,----------,----------,----------,----------,----------,----------,----------,----------,----------
4,,G2,,,D5 amp:0.2,,,,,,,,
5,,,,,,,,,,,,,
6,,,,,,,,,,,,,
7,,,,,C#5 amp:0.2,,,,,,,,
```

The header row should consist of the following columns:

* `#` - cells within this column contain the line number
* `Master` - cells within this column contain project-wide settings, such as:
  + `bpm:<double>` - the beats per minute [default: 60]
  + `lpb:<double>` - the number of tracker lines (rows) per beat [default: 16]
  + `tn:<string>` / `tuning:<string>` - the tuning [default: "a440"]
  + `tmp:<string>` / `temperament:<string>` - the temperament [default: "12tet"]
* Any instrument names written in the following format:
  1. An optional "M" or "S" prefix, in order to mute or solo the track, 
     respectively
  2. The letter "I" (short for "Instrument"), followed by the instrument number
     referenced in `instruments.json`
  3. An optional space, followed by an optional name

Cells within instrument columns can be blank or can contain one of the 
following:

1. A pitch name followed by its octave number (e.g., "Bb4")
2. The value "OFF" - this is used to turn the prior note off

Notes are held (i.e., across blank cells) until another pitch or the word "OFF"
is encountered.

Blank cells or cells with pitches can also contain effects. An effect is a 
key-value pair, where the key is the name of an instrument parameter (or an 
alias listed in `parameterRenamings`) within `instruments.json`, and the value
is a number.

A row can begin with a hyphen (`-`); this can be helpful for visually marking
sections within a composition.

## Sheet Music Notation

### LilyPond

A prior iteration of Schisma had a [LilyPond](https://lilypond.org/) parser. 
Its development stalled when it became apparent that conventional notation has
serious limitations when handed off to a computer for playback.  There is a 
[ticket](https://gitlab.com/schisma/schisma/-/issues/3) to restore it, but more
ideation is required (especially around performance notation) before it will
achieve parity with the [tracker](#tracker).

## Composition Configuration

Composition-wide settings can be configured in a JSON file. Here are the
contents of the
[Commemoration example](https://gitlab.com/schisma/schisma/-/blob/master/examples/Commemoration/composition.json):

```json
{
  "trackerSettings": {
    "parameterRenamings": {
      "aa": "amplifierAttack",
      "rel": "amplifierRelease",
      "amp": "amplitude"
    },
    "frequencyMapper": {
      "name": "default",
      "arguments": []
    }
  }
}
```

The top-level keys are:

* `trackerSettings` - an object containing the tracker settings
  + `parameterRenamings` - an object containing parameter aliases that can be
    used within the [tracker](#tracker)
* `frequencyMapper` - an object containing the name of the
  [frequency mapper](#frequency-mappers), along with the arguments to be passed
  to it

### Frequency Mappers

Currently, there are two supported frequency mappers:

* `default`: The default frequency mapper will convert pitches using the
  specified tuning and temperament
* `withRandomDetuning`: This frequency mapper will convert pitches using the
  specified tuning and temperament, and then randomly detune each pitch by a
  maximum number of cents (specified as the first argument to `arguments`)

## Project File

The project file ties together the instruments file, the tracker file, and
the composition file into one "project". Here are the
contents of the
[Commemoration example](https://gitlab.com/schisma/schisma/-/blob/master/examples/Commemoration/project.json):


```json
{
  "compositionFile": "./composition.json",
  "instrumentsFile": "./instruments.json",
  "trackerFile": "./tracker.csv"
}
```

Both absolute and relative filenames are supported.

# Output Methods

## Csound

Currently, Schisma only supports output via Csound. All notation and 
instruments are compiled down to Csound code and then run automatically.

Listen to an example score below. (It's a good idea to lower your volume 
first!)

```sh
schisma tracker play -p examples/Commemoration/project.json -s 0 -e 89
```

The ability to render to an audio file will be available shortly. For now, you
can edit the resulting Csound file (located at `/tmp/tracker.csd`):

```csd
; Replace this line
-odac -+rtmidi=portmidi -Ma
; with this:
-o /tmp/tracker.wav
```

And then render `/tmp/tracker.wav` by running Csound:

```sh
csound /tmp/tracker.csd
```

# Interoperability

## MIDI

Schisma supports MIDI input. Run the following in GHCi to play around with a
simple sine wave from a virtual MIDI keyboard:

```hs
let osc = oscilWithDefaults (i# 1) cpsmidi :: ARateSignal
playWithVirtualMidi empty [osc, osc] 
```

If you have a physical MIDI device, try the following:

```hs
let osc = oscilWithDefaults (i# 1) cpsmidi :: ARateSignal
playWithMidiDevice empty [osc, osc]
```

Or try taking the "Profit" synthesizer for a spin using its default settings:

```hs
let p = Schisma.Synth.Emulations.Profit.profit (Data.Map.Strict.fromList [("amplitude", i# 1), ("frequency", cpsmidi)])
playWithMidiDevice empty [p, p]
```

# Available Instruments

* [Profit](https://gitlab.com/schisma/schisma/-/blob/master/src/Schisma/Synth/Emulations/Profit.hs) - based
  on the highly influential [Prophet-5](https://en.wikipedia.org/wiki/Prophet-5)
* [SoundFont](https://gitlab.com/schisma/schisma/-/blob/master/src/Schisma/Synth/SoundFont.hs) - a 
  generic SoundFont player, capable of loading any `.sf2` file

## Parameters

Running the following will print out a list of instrument (synth) names along 
with information about their parameters:

```sh
schisma synth list
```

# GUI

Plaintext not for you? Check out the [GUI](https://gitlab.com/schisma/gui), 
which can assist greatly with notation and instrument design (as parameters can
be tweaked on the fly).

# Documentation

The codebase has been meticulously documented (particularly around the Csound 
opcodes). View it with:

```sh
stack haddock schisma --open
```

# Tests

Run them with:

```sh
stack test
```

# Credits

A world of gratitude is owed to the folks behind Csound. It is a truly amazing
piece of software and the community around it is incredible.

Special thanks to Paul Hudak, whose lucid writing in
[The Haskell School of Music](https://www.cs.yale.edu/homes/hudak/Papers/HSoM.pdf)
helped kickstart the ideas in this project.

And the biggest thanks of all to DR, whose poking and prodding at my musical
discontents made all of this possible.
