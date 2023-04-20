# Stage 4

### Exercise 1

## Replacements for common letters 1/2

### Greediness 2

In the exercises of the last stage, you learned ground rules.
They are ground rules, because they are marked "Greediness 0", G0 in short.
The idea behind the G0 rules, is
that any word that you ever need to write has at least one valid entry in the steno dictionary.
And on top of this, alternative steno spellings are added.

The G0 codes are generally not the most efficient available steno code.
An example: In the last stage you learned that t is typed using `D+` or `+D`, respectively.
However, take the word «gut» and you will notice
that there isn't really a good reason to bother with the `+`
as the steno code `GUD` isn't occupied by any valid German word.
This is one of the implicit advantage of any steno-style system:
When typing in the conventional, serial style, every input is valid.
When typing steno chords, we can recycle otherwise nonsensical inputs to increase efficiency.

Any ruleset, like the one presented below, that has a Greediness bigger than 0
is a set of optional rules.
All the words that you learn in this exercise already have a G0 code in the plover dictionary.
Being greedy means, taking a shortcut:
By cutting short the steno codes, the result is more efficient typing.

But we have to be careful with greediness.
If you want to type «Rat» you still need the G0 code,
as `RAD` results in «Rad», which is a perfectly valid German word.
In cases like this, the G0 rules take precedence.
There are exceptions, but in practice you don't need to concern yourself with any of this.
The words that you learn in the exercises here, always appear with their most efficient steno code possible.
That means: the most greedy steno code that is not reserved already by another word
that appears more frequently in the German language.
The plover dictionary contains all valid steno codes,
such that accidentally using a less-then-optimal steno code isn't a problem when using plover.

<!--separator-->

Now there is a bit of a challenge regarding the optimal learning path.
On the one hand, learning all the G0 rules first is a safe path
to learning a valid steno code for any conceivable word,
before you concern yourself with any optional rules for efficiency.
On the other hand, it would be nice to learn the most common words first—
along with their most efficient steno code.
This way, you may find yourself advancing much quicker.

My proposed solution—if you follow the exercises in order—
is to interweave the greedier rules in between the G0 rules.
The rules that cover a high number of words,
regardless of their greediness, will appear early on.
