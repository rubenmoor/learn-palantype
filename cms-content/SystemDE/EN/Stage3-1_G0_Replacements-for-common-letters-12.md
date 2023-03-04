# Stage 3

### Exercise 1

## Replacements for common letters 1/2

### Greediness 0

In general, any word in the natural language translates to some steno code based on a couple of straightforward substitutions.
In this exercise, we start with the most common ones.

<!--separator-->

First of all, note that these patterns are in addition to the simple patterns of the previous [Exercise 2.3](DE/11).
Thus, if you are missing a letter, it might be among the simple patterns.
Also, note that the minus sign, `-`, isn't an actual steno key.
Instead, it is used to distinguish between the left-hand and right-hand version of keys
that appear twice on the steno keyboard.
`-LS` simply refers to `L` and `S` of your right hand.
`L-S` would refer to the `L` of your left hand and the `S` of your right hand.
Finally, in order to refer to `S` and `L` of your left hand, the code would look like this:
`SL-`, always obeying the proper order of steno keys.

This is a lot to memorize, right from the start.
Take your time to discover some regularities.
E.g. the `+` turns *g*, *d*, and *b* into *k*, *t*, and *p*, respectively.
The *x* looks weird but it really is simply typed by `DSG` with the left hand,
which becomes `GSD` with the right hand.
