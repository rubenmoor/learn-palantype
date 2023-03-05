# Formatting input

## On our way to actual typing

Learning to type individual words is fun and all, but so far we are still missing the basics of formatting.
The exercises of Stage 15 are all about that.
Let's close some gaps to get closer to productive use of our palantype-style steno skills.

### Punctuation

The list below is long. Luckily you won't have to remember all of the commands.
Rather, learn them as you feel the necessity.

Most importantly, use `A` for comma, `N-` for full stop, and `B-` to capitalize last word retroactively.
This last one is important because Plover will try to handle capitalization for you.
This means the first word of each sentence will be capitalized and usually nouns will be.
However, whenever a German word does not solely exists as noun, the system will output the word uncapitalized and leave the decision to you.

| Short | Key    | Description                       | Plover code     |
|-------|--------|-----------------------------------|-----------------|
| ,     | `A   ` | attach comma                      | {^,}            |
| ;     | `NA  ` | attach semicolon                  | {^;}            |
| -     | `~   ` | hyphen to attach words            | {^-^}           |
| \\t   | `DJ  ` | tab like t                        | {^\t^}          |
|       | `B-  ` | capitalize last word              | {\*-&#124;}     |
|       | `D-  ` | capitalize next word              | {-&#124;}       |
|       | `S-  ` | uncapitalize last word            | {\*&gt;}        |
| ␣     | `G-  ` | retroactively add space           | {\*?}           |
|       | `J   ` | paragraph: period, two newlines   | {^. ^}{-&#124;} |
|       | `F-  ` | retroactively delete space        | {*!}            |
| .     | `N-  ` | full stop: period                 | {^.}{-&#124;}   |
| :     | `L-  ` | attach colon                      | {^:}            |
| :     | `JL- ` | colon and capitalize              | {^:}{-&#124;}   |
| ?     | `JN- ` | question mark and capitalize      | {^?}{-&#124;}   |
| !     | `JR  ` | exclamation mark and capitalize   | {^!}{-&#124;}   |
| #     | `H   ` | hash with next word attached      | {\#^}           |
| §     | `BD- ` | legal paragraph symbol            | §               |
| °     | `GD- ` | attach degree symbol              | {^°}            |
| ™     | `DM- ` | attach trademark symbol           | {^™}            |
| ©     | `GDM-` | attach copyright symbol           | {^©}            |
| €     | `E   ` | euro symbol                       | €               |
| —     | `~Ü  ` | em dash                           | —               |
| s     | `s   ` | attach s and attach the next word | {^s^}           |

### Opening and closing

The commands that follow adhere to a strict logic and thus aren't that scary.
Your fingers of your right hand take care of any thing that opens and closes.

| Short | Description     | Key 1   | Key 2 | Plover code |
|-------|-----------------|---------|-------|-------------|
| «»    | Guillemets      | `+ `    | `-G`  | {«^},{^»}   |
| „“    | German quotes   | `-L`    | `-N`  | {„^},{^“}   |
| ‹›    | chevrons        | `-M`    | `-B`  | {‹^},{^›}   |
| []    | square brackets | `-F`    | `s `  | {[^},{^]}   |
| ()    | parenthesis     | `-S`    | `-D`  | {(^},{^)}   |
| {}    | brackets        | `ʃ `    | `n `  | {\{^},{^\}} |

### ASCII art

Steno typing lends itself well to all kinds of macros,
among them I included my personal selection of ASCII emoticons.

|         |               |
|---------|---------------|
| `SLNSD` | `¯\_(ツ)_/¯`     |
| `BLNSD` | `ʕ•ᴥ•ʔ`         |
| `GLNSD` | `(´･_･`)`       |
| `HLNSD` | `(⊃｡•́‿•̀｡)⊃`   |
| `DLNSD` | `(╯°□°）╯︵ ┻━┻`  |
| `FLNSD` | `(☞ﾟヮﾟ)☞`       |
| `MLNSD` | `(๑•́ ₃ •̀๑)`   |
| `JLNSD` | `┬─┬⃰͡ (ᵔᵕᵔ͜ )` |
| `WLNSD` | `( ˘ ³˘)♥`      |
| `LLNSD` | `( ͡° ͜ʖ ͡°)`   |
| `NLNSD` | `( ಠ ʖ̯ ಠ )`    |
| `RLNSD` | `(ᵔᴥᵔ)`         |

### Plover application shortcuts

When actually using the Plover software instead of practicing on this website,
you will find those shortcuts useful.
They all follow the same scheme: `BDJN` plus some letter related to the plover command.

|          |                          |
|----------|--------------------------|
| `BDJN+D` | {PLOVER:TOGGLE}          |
| `BDJNA ` | {PLOVER:ADD_TRANSLATION} |
| `BDJNL ` | {PLOVER:LOOKUP}          |
| `BDJNS ` | {PLOVER:SUGGESTIONS}     |
| `BDJNF ` | {PLOVER:FOCUS}           |
| `BDJNG ` | {PLOVER:CONFIGURE}       |
| `BDJN+G` | {PLOVER:CONFIGURE}       |

Sooner or later you will add your own dictionary entries, or make adjustments to the default.
The [Plover syntax](https://github.com/openstenoproject/plover/wiki/Dictionary-Format)
is documented well for exactly that purpose.

### Practicing for the real-life

There are more interactive exercises on this page, however, if you have not already done so,
it is high time you actually install the
[Plover Software](https://github.com/openstenoproject/plover/releases/tag/v4.0.0.dev10).
Plover Version 4 comes with a Plugin Manager where you can find "Palantype DE".
Try it out and type a few paragraphs!
