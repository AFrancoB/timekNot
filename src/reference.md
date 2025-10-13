# TimekNot: a poly-temporal weirdo-lang for your time-oriented music explorations

## Rhythmic notation

### X/O notation.

The X/O notation has some advantages: 

- Makes the rhythmic idea very explicit and visible.
- Separates the sound parameters from the temporal ones. This will be important in the near future for timeknit.
- The position of the 'x' and 'o' keys in many keyboards can allow the performer to tap the rhythm desired using body gestures. No, there is no computational trick involved here, just a way to tap your rhythms (aka, feeling them in your body) rather than xxxxxxxxx.

This is the most basic rhythm notation program:

`x` 

This will produce a single event with a duration of 1 tempo unit. If the tempo is 120 bpm the 1/4, each x will trigger an event with such duration. The `o` indicates an event that is silent. So the following program indicates a pattern of silent and sounding events:

`xoxo`

This program has a duration of 4 events lasting each one tempo unit. So the following program is shorter in duration than the last one:

`xox`

Space is meaningless, only helpful to organise ideas. 

`xxoxooxx` is equivalent to `xxox ooxx`

### Subdivisions

The x/o notation at the top level is additive. This means that any new onset represented by a `x` or `o` will make the duration of the musical idea longer. If a divisive paradigm is required the notation requires brackets:

`[xxoxo]`

Anything between brackets will have a total duration of 1 tempo unit. Each event of the program example above will have a duration of 1/5 of one tempo unit. 

The subdivision can be recursive:

`[xxx[ox]]`

The total duration of this program is 1 tempo unit. The duration of the three `xxx` is 1/4 and the total duration of the events in the second bracket ar 1/4 as well. The duration of each event in the inner bracket is of 1/8 each.

As can be observed in the previous example, this notation can be combined with simple X/O notation:

`xx[ox]x`

### Repetition

To notate a repeating pattern this is the notation:

`!xxox#5`

This will repeat the pattern `xxox` five times. With a total duration of number of events of the pattern times repetition number. In this case (4*5) 20 tempo units.

This notation can be combined with subdivisions in two forms:

`![xxox]#8`

Or

`[xxxx !ooxo#3 ooox]`


### Euclidean distribution

The notation to produce euclidean distribution can be represented in the following ways:

`(xx,k,n,off)`

Where k is an `int` representing the number of times the pattern event is triggered, the n is an `int` that represents the number of units in the cycle where k is distributed and the off (also an `int`) that represents an offset value.

`(xx,ox,k,n,off)` 

The added pattern as second argument will fill the n points of the cycle that are not k. Patterns can have different durations.

`(k,n,off)` 

This will be equivalent to `(x,k,n,off)`.

The offset value is optional:

`(xx,ox,k,n)`

The euclidean pattern can become a subdivision if its written between brackets. Also can be combined with the repetition notation:

`[(3,8)]`

`!(5,8)#4`

## Marking loops VS one-time ideas

To mark the end of the rhythmic notation from the rest of the program I am using two separators:

`:|` Which will loop the rhythmic idea.

and

`||` Which will produce the rhythmic idea once. Normally this separator has to be used with the `eval` mode in order for the output to be predictable. If it is used in `origin` mode, the program will have happened at the point in time where the time-keeping engine being used marks the start of the beat count. 

## Polytemporal functions

`canonise 20 [1,2,3,4]`

Will create a tempo canon (this means, replicating the model idea with different tempo proportions), the convergence point will be 20 events from evaluation time and the voices will have the proportions indicated in the brackets.

`canonise 20 [1,2,3,4] # transposeSamples "bd cp bd"`

`canonise 20 [1,2,3,4] # transposeN [1,2,3]`

`canonise 20 [1,2,3,4] # transposePan [0,1,0]`


## Sound functions

`seqEvent` Makes a sequence of events. 

`xxox :| seqEvent $ s "bd bd cp bd"`  

`seqMetre $ s "bd bd cp bd"'` Makes a sequence of metres.

`seqRefrain $ s "bd bd cp bd"` Makes a sequence of refrains.


## Future explorations

Can the `o` of the binary rhythmic notation represent another layer of sound rather than silence? what would that look like?

Incomplete subdivisions. I can have a notation like `[xox/..7` which would mean that the duration is determined by an underlying structure like `[xoxoooo]` but only three events are computed. Assuming this example is on the top layer of the parser this will mean that three events are created with a duration of 1/7 tempo units.

Perhaps two new separators that allow players to only loop certain parts of the musical refrain. Like:

`xox xx[ox] [xxxx] |: xxxx xox :|`