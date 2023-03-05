# Stage 5

### Exercise 1

## R in the onset

### Greediness 0

*R* is quite a common letter in German, still it is missing from the keyboard.
To see why, note that in German a syllable combines a lot of consonants.
In the onset, there are *dr*, *tr*, *schr*, *fr*, *gr*, *kr*, *br*, and *pr*, and even *str*, *spr*.
The straightforward way to implement *R* thus would be an R-key on the index finger of the left hand,
to the right of all the other keys, including `+`.
You see, we are simply running out of space and need a different solution.
The basic idea is that the M-key and the L-key fill the role of *r* in the onset.
They can't combine with *r* themselves, which is fine.
In particular, you use `M` in the most simple case and `L` to type *tr* and *spr*.
Unfortunately, this is not enough and we need to add `D` as a replacement for `+` in a couple of cases.

<!--separator-->

And don't forget: The *r* without any other consonants exists, too.
