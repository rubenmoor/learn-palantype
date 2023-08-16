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
However, whenever a German word does not solely exists as noun,
the system will output the word uncapitalized and leave the decision to you.

| Short | Key    | Description                       | Plover code     |
|-------|--------|-----------------------------------|-----------------|
| ,     | `A   ` | attach comma                      | {^,}            |
| ;     | `+A  ` | attach semicolon                  | {^;}            |
| -     | `~   ` | hyphen to attach words            | {&Hat;-&Hat;}           |
| \\t   | `D+- ` | tab like t                        | {&Hat;\t&Hat;}          |
|       | `D-  ` | capitalize last word              | {\*-&#124;}     |
|       | `S-  ` | capitalize next word              | {-&#124;}       |
|       | `v   ` | uncapitalize last word            | {\*&gt;}        |
| ␣     | `b   ` | retroactively add space           | {\*?}           |
|       | `N-  ` | paragraph: period, two newlines   | {^. ^}{-&#124;} |
|       | `F-  ` | retroactively delete space        | {*!}            |
| .     | `+-  ` | full stop: period                 | {^.}{-&#124;}   |
| :     | `M-  ` | attach colon                      | {^:}            |
| :     | `NM- ` | colon and capitalize              | {^:}{-&#124;}   |
| ?     | `N+- ` | question mark and capitalize      | {^?}{-&#124;}   |
| !     | `NL- ` | exclamation mark and capitalize   | {^!}{-&#124;}   |
| #     | `N+- ` | hash with next word attached      | {\#^}           |
| §     | `B+- ` | legal paragraph symbol            | §               |
| °     | `G-  ` | attach degree symbol              | {^°}            |
| ™     | `DM- ` | attach trademark symbol           | {^™}            |
| ©     | `DʃG-` | attach copyright symbol           | {^©}            |
| €     | `E   ` | euro symbol                       | €               |
| —     | `~Ü  ` | em dash                           | —               |
| s     | `s   ` | attach s and attach the next word | {^s^}           |

### Opening and closing

The commands that follow adhere to a strict logic and thus aren't that scary.
Your fingers of your right hand take care of any thing that opens and closes.

| Short | Description     | Key 1   | Key 2 | Plover code           |
|-------|-----------------|---------|-------|-----------------------|
| «»    | Guillemets      | `-MG `  | `-ʃn` | {«&Hat;},{&Hat;»}     |
| „“    | German quotes   | `-+`    | `-N`  | {„&Hat;},{&Hat;“}     |
| ‹›    | chevrons        | `-L`    | `-B`  | {‹&Hat;},{&Hat;›}     |
| []    | square brackets | `-ʃ`    | `n`   | {[&Hat;},{&Hat;]}     |
| ()    | parenthesis     | `-S`    | `-D`  | {(&Hat;},{&Hat;)}     |
| {}    | brackets        | `-M`    | `-G`  | {\\{&Hat;},{&Hat;\\}} |
| "     | double quotes   | `-+N`   | `-SD` | {"&Hat;},{&Hat;"}     |
| '     | single quotes   | `-LB`   | `-Fs` | {'&Hat;},{&Hat;'}     |
| &#96; | backticks       | `-+NS`  | `-NSD`| {&#96;&Hat;},{&Hat;&#96;}     |

The `"`, `'`, ``` ` ``` are added for the convenience.
Plover allows to attach the character to the next and to the previous word, respectively.
There is nothing wrong with using the fingerspelling version of `"`, `'`, and `&#96;`,
you find them in [Ex. 15.5: Special Character](SystemDE/TextEN/57).
Only in that case you have to put the space explicitly, too.

### ASCII art

Steno typing lends itself well to all kinds of macros,
among them I included my personal selection of ASCII emoticons.

|         |               |
|---------|---------------|
| `v-+NSD` | `¯\_(ツ)_/¯`     |
| `D-+NSD` | `ʕ•ᴥ•ʔ`         |
| `b-+NSD` | ```(´･_･`)```       |
| `ʃ-+NSD` | `(⊃｡•́‿•̀｡)⊃`   |
| `S-+NSD` | `(╯°□°）╯︵ ┻━┻`  |
| `F-+NSD` | `(☞ﾟヮﾟ)☞`       |
| `G-+NSD` | `(๑•́ ₃ •̀๑)`   |
| `N-+NSD` | `┬─┬⃰͡ (ᵔᵕᵔ͜ )` |
| `B-+NSD` | `( ˘ ³˘)♥`      |
| `M+NSD` | `( ͡° ͜ʖ ͡°)`   |
| `++NSD` | `( ಠ ʖ̯ ಠ )`    |
| `L+NSD` | `(ᵔᴥᵔ)`         |

### Plover application shortcuts

When actually using the Plover software instead of practicing on this website,
you will find those shortcuts useful.
All but the first follow the same scheme: `DSN+-` plus some letter related to the plover command.

|          |                          |
|----------|--------------------------|
| `I+NSD`  | =undo                    |
| `DSN++D` | {PLOVER:TOGGLE}          |
| `DSN+A`  | {PLOVER:ADD_TRANSLATION} |
| `DSN+L`  | {PLOVER:LOOKUP}          |
| `DSN+-S` | {PLOVER:SUGGESTIONS}     |
| `DSN+-F` | {PLOVER:FOCUS}           |
| `DSN+-G` | {PLOVER:CONFIGURE}       |
| `DSN++G` | {PLOVER:CONFIGURE}       |
|          |                          |

Sooner or later you will add your own dictionary entries, or make adjustments to the default.
The [Plover syntax](https://github.com/openstenoproject/plover/wiki/Dictionary-Format)
is documented well for exactly that purpose.

### Practicing for the real-life

There are more interactive exercises on this page, however, if you have not already done so,
it is high time you actually install the
[Plover Software](https://github.com/openstenoproject/plover/releases/tag/v4.0.0.dev10).
Plover Version 4 comes with a Plugin Manager where you can find "plover-palantype-DE".
Try it out and type a few paragraphs!
