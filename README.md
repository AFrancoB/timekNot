TimekNot
==============
***Polytemporal language for Live Coding and Tempo Music Experimentation***

**Author:** *Alejandro Franco Briones*


# Temporal Expressions

These expressions have an identifier, a tempo mark and a rhythm expression:

~~~~
myClock 120cpm | xxox :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = 0 2 4; -- we will ignore this line for now
~~~~

`myClock` is an arbitrary *identifier*. Just create a name for your time expression.

`120cpm` is the *tempo mark*. In this case cycles per minute. 
Other ways to express tempo are:
`cps` (cycles per second). Assuming `2cps`, it is equivalent to `(2*60)cpm`. 
`tl` a shortcut for slow units (1/4 of the speed of a CPM) very useful in live coding and polytemporal settings. Assuming `480tl`, it is equivalent to `(480/4)cpm`. 

`| xxox :|` is the rhythmic expression. 

`xxox` is a *rhythmic block*: a sequence of *time units* expressed as onset (`x`) or offsets (`o`). The duration of the time unit (TU) is determined by the tempo mark. Each TU at `120cpm` has a duration of 0.5 seconds. The rhythmic block of the example above will have a total duration of 2 seconds.

 Notice the `:|`, this indicates that the rhythmic block will be looped, if `||` is used instead, the rhythmic block will not be looped. The `|` at the begining of the rhythmic expression separates it from the rest of the temporal expression.

## Rhythmic Notation

### XO Notation

`x` onset, `o` offset. This means that the program will instantiate as an aural expression anything that is `x`, and ignore everything that is a `o`.

### Subdivision

Another way to represent a time unit is with `[]`. So `xx[ox]x` will still have a duration of 2 seconds, but the third TU is subdivided into 2 sub-units wich duration is: TU/(NumberOfSubUnits). In our example, if the TU has a duration of 0.5 seconds, the sub-unit will have a duration of 0.25.

This notation is recursive:

~~~~
myClock 120cpm | xx[o[x[ox[xx]]]]x :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = 0 2 4; -- we will ignore this line for now
~~~~

### Repetition

~~~~
myClock 350cpm | ![xx][ox]x#3 [oxx] :|
~~~~

This notation is a shortcut to create longer *rhythmic blocks*. Notice the `!` at the begining o what needs to be repeated and the suffix is a `#` with the amount of repetitions required. The block above is equivalent to:

~~~~
myClock 350cpm | [xx][ox]x [xx][ox]x [xx][ox]x [oxx] :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = 0 1 3; -- we will ignore this line for now
~~~~

### Bjorklund 

Euclidean structure can be notated as follows:

Simple:
`(3,8)` will produce `xooxooxo`

With rotation:
`(3,8,1)` will produce `ooxooxox`

With a partial pattern:
`(xx,3,8)` will produce `xxooooxxooooxxoo`

With a full pattern:
`(x[xx],o[ox], 3,8)` will produce `x[xx] o[ox] o[ox] x[xx] o[ox] o[ox] x[xx] o[ox]`

Some rhythmic possibilities:

~~~~
myClock 400cpm | !([x[xx]],[x[ox]], 3,7)#2 ([[xx]x], [xox], 5,8) [(3,7)] [([xx],2,5)] :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = 0 1 3 2 5 7 6 5; -- we will ignore this line for now
~~~~

# Aural Expressions

All aural expressions need to invoke an identifier created with a temporal expression and a chain of messages that function as audio parameters based on Dirt, SuperDirt, and WebDirt.

~~~~
myClock 200cpm | (5,8) [xx] ([ox],[xx],3,7) :|

myClock.sound = _ "muted bright";
~~~~

The identifier `myClock` is followed by the message `sound`. This is the *parameter* to be assigned. The parameters can be chained and the list of parameters is:

- `sound`. Synonym `s `. This assigns the sample library.

- `n`. Assigns the index of the sample from the sample library.

- `gain`. Controls gain from 0 to 1.

- `pan`. Controls panning from 0 to 1.

- `begin`. Position of the sample to begin reproduction (0 to 1).

- `end`. Position of the sample to end reproduction (0 to 1).

- `legato`. Modifies the aural event's length.

- `speed`. Control the speed of reproduction of the sample. 

- `vowel`. Formant filter, values: `a`, `e`, `i`, `o` & `u`.

- `orbit`. Assigning different audio channels in SuperDirt.


For now, using Estuary allows you to connect with SuperDirt as well. Need to install and enamble SuperDirtSocket found [here](https://github.com/dktr0/superDirtSocket).

You can determine how are the aural events distribute in relation to the rhythmic block with *span*:

`_` or `:isorhythmic` distributes them isorhythmically regardless of the block length (reminiscent of SuperCollider’s pattern library).

` _-_` or `:spread` spreads them evenly (in the style of TidalCycles)

`_-` or `:isorhythmicBlock` creates block-long isorhythms, repeating each value within the block and changing value when a new iteration of the block appears.

Span has `_` as default value, it can be omitted. 

After span, players need to provide a list of values. Sound, exceptionally requires quotations around the value list. So:

~~~~
myClock 300tl | xxox :|

myClock.s = "muted sankoor muted" .n = 0 2 4 3 1;
~~~~

This produces an isorhythmic sequence as follows:

Like this:

| val | X     | X     | O     | X     | X     | X     | O     | X     | etc... |
|-----|-------|-------|-------|-------|-------|-------|-------|-------|--------|
| s   | muted |sankoor| muted | muted |sankoor| muted | muted |sankoor| etc... |
| n   | 0     | 2     | 4     | 3     | 1     | 0     | 2     | 4     | etc... |


The notation for this section will drastically improve soon...

## Dastgah

The Iranian tuning system is implemented directly in the aural expressions. These modes can be invokes as aural parameters:

- Shur
- Segah
- Nava
- Homayun
- Chahargah
- Mahur
- RastPanjgah

~~~~
myClock 300cpm | !([x[xx]],[x[ox]], 3,5)#2 ([[xx]x], [xox], 5,7) [(3,5)] [([xx],2,5)] :|

myClock.s = "muted" .n = 4 3 2 1 0 .homayun = 0 1 3 2 5 7 6 5 4;
~~~~

Some of these modes have 'changeable notes' or 'Moteghayyer' notes, like:

~~~~
myClock 500cpm | x :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = 0 1 2 3 4 5 6 7 6 5 4 3 2 1;
~~~~

Shur's sixth degree is tuned differently depending the melodic direction. Iranian modal functions can be expressed with a character rather than a note value:

~~~~
myClock 500cpm | x :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = a f 2 3 4 m 6 7 6 5 4 3 2 1;
~~~~

The functions are: 

- Āghāz (beginning), notated as `a`
- Finalis, destination of the melodic cadence, notated as `f`
- Ist (stop), as `i`
- Shāhed (witness), as `s`
- Moteghayyer (changeable), as `m`

Anybody better familiarised with this system that desires to discuss a more precise implementation please keep in touch!

# Tuning Expression

Beyond the Dastgah programmed as foundational part of TimekNot, pitch can be modified in two manners. With Combinatorial Product Sets and drawing from Scala files.

All tuning expressions start with an identifier followed by `:`. They all end with a `;`.

## Just Intonation: Combinatorial Product Sets (CPS) and Scala Format

### CPS

The notation is simple:

~~~~
myCPS : cps 2 (1 3 5 7 11);
~~~~

After the identifier and the `:`, you can write the function cps that takes two arguments: The first one represents the number of values to multiply together from the set, and the second one is the set itself. 

### Scala

After the identifier and the `:`, write `scala` and a `:`. The you write the cents from the root or the ratio representing each degree of a scale like this:

~~~~
--c1/4-comma mean-tone scale. Pietro Aaron's temperament (1523).
-- retreived from https://huygens-fokker.org/scala/examples.html
myScala : scala : 76.049 193.157 310.265 5/4 503.422 579.471 696.578 25/16 889.735 1006.843 1082.892 2/1;
~~~~

Then invoked as an aural expression:

~~~~
myClock 500cpm | x :|

myClock.s = "bright" .n = 4 3 2 1 0 .myScala = 0 4 7 12 0 5 9 12 2 7 11 12;

myScala : scala : 76.049 193.157 310.265 5/4 503.422 579.471 696.578 25/16 889.735 1006.843 1082.892 2/1;
~~~~

Notice how the first degree is omitted. This parser for now only accepts floats or ratios expressed like n/m.

# Combining Temporal Expressions

So far, all temporal expressions have been anchored to an external tempo keeping mechanism (see Tempi Library) that determines their starting point. Thus, all expressions aligned to the external tempo mechanism share the same point of alignment, that is why the two expressions below sound aligned:

~~~~
myClock 200cpm | xo :|

myTempo 200cpm | xo :|

myClock.s = "muted" .n = 2;

myTempo.s = "muted";
~~~~

However, TimekNot's most relevant characteristic is the capacity to use expressions as anchors to align other temporal expressions. The second expression below has `myTempo` as an identifier and the operator `><` indicate that it will *converge* with `myClock`. In the example below `myTempo` is aligned with `myClock` at both expression's first events. 

~~~~
myClock 200cpm | xo :|

myTempo >< myClock 200cpm | xo :|

myClock.s = "muted" .n = 2;

myTempo.s = "muted";
~~~~

We can change the moment of alignment using the syntax provided for that:

~~~~
myClock 200cpm | xo :|

myTempo[1] >< myClock[4] 200cpm | xo :|

myClock.s = "muted" .n = 2;

myTempo.s = "muted";
~~~~

In the example above, `myTempo`'s second event is aligned with `myClock`'s fifth event (count starts at 0). Thus, now an `x` in the former converges with an `o` in the latter. 

The idea behind TimekNot is to be able to have this convergence points somewhere closer to the present. In other words, the idea is to bring this convergence point to the performance time. For that the `>>` or (after evaluation) can be invoked. `>>` will add the amount of rhythmic blocks elapsed until evaluation to the number in square brackets in the identifier after `><`. So, `myTempo[1] >< myClock[4>>]` will create a convergence point 1 rhythmic block + 5 events after evaluation time.   

~~~~
myClock 200cpm | xo :|

myTempo[1] <- myClock[4>>] 200cpm | xo :|

myClock.s = "bright";

myTempo.s = "muted";
~~~~

# (General) Expressions

The tempo mark parser and the transposition parser can handle basic arithmetics. This behaviour will be expanded and generalised wherever poossible, but for now they can support the creation of multiple tempo marks in temporal expressions and transposition values in aural expressions.

~~~~
myClock 13cpm*2^2 | x[xx] :| 

myTempo[1] <- myClock[4>>] 200cpm | xo :|

myClock.s = "bright";

myTempo.s = "muted" .shur = 0 + [0,1,3,4,2];
~~~~


# Tempo Canon Idiom

A tempo canon takes an array of *tempi* rather than one tempo mark.

~~~~
myClock [120cpm,60cpm,180cpm,40cpm] | xxox :|
~~~~

In order for this to have any audible effect, the aural expressions need to have at least one parameter expressed with transposition values:

~~~~
myClock [120cpm,60cpm,180cpm,40cpm] | xxox :|

myClock.s = "muted" .n = 4 3 2 1 0 .shur = 0 2 4 + [0,2,4,6]; 
~~~~

It is possible to calculate a tempo canon in the near future:

~~~~
myClock [120cpm,60cpm,180cpm,40cpm]*2 | x :|
myClock.s = "sankoor" .n = 4 3 2 1 0 .shur = 0 2 4 + [0,2,4,6]; 

myTempo[10] >< myClock-2[10>>] 30cpm*[10,11,13,17,21] | x :|
myTempo.s = "sankoor" .n = 4 3 2 1 0 .shur = 1 3 0 + [0,1,5,3,(-2)]; 
~~~~

Note that the new convergence is with `myClock-2`, which is the layer with a tempo of `180cpm`.

There are additional ways to create a tempo canon:

~~~~
myClock 120cpm*[1,1.1,1.2,1.3,1.4] | xxox :|
~~~~

There is also the `til` function:

~~~~
myClock 100cpm*(1,1.01 til 2) | x :|
~~~~

That can also be used in an  aural expression with transposition:

~~~~
myClock 100cpm*(1,1.01 til 2) | x :|
myClock.s = "sankoor" .n = 4 3 2 1 0 .shur = 0 2 4 + (0 til 10); 

myTempo[10] >< myClock[6>>] 30cpm*[10,11,13,17,21] | x :|
myTempo.s = "sankoor" .n = 4 3 2 1 0 .shur = 1 3 0 + [0,1,5,3,(-2)]; 
~~~~